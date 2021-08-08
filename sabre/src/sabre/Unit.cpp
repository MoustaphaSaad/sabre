#include "sabre/Unit.h"
#include "sabre/Scan.h"
#include "sabre/Parse.h"
#include "sabre/Check.h"
#include "sabre/Reflect.h"
#include "sabre/GLSL.h"

#include <mn/Path.h>
#include <mn/IO.h>
#include <mn/Log.h>
#include <mn/Defer.h>
#include <mn/Json.h>

namespace sabre
{
	inline static void
	_push_type(mn::Set<Type*>& types, Type* t)
	{
		if (mn::set_lookup(types, t))
			return;

		switch (t->kind)
		{
		case Type::KIND_STRUCT:
		{
			for (const auto& field: t->struct_type.fields)
				_push_type(types, field.type);
			break;
		}
		default:
			break;
		}
		mn::set_insert(types, t);
	}

	// API
	Unit_File*
	unit_file_from_path(const mn::Str& filepath)
	{
		auto absolute_path = mn::path_absolute(filepath);
		auto self = mn::alloc_zerod<Unit_File>();
		self->absolute_path = absolute_path;
		self->filepath = clone(filepath);
		self->content = mn::file_content_str(filepath);
		self->ast_arena = mn::allocator_arena_new();
		return self;
	}

	void
	unit_file_free(Unit_File* self)
	{
		mn::log_debug(
			"File '{}': AST {}/{}, (used/reserved)bytes",
			self->absolute_path,
			self->ast_arena->used_mem, self->ast_arena->total_mem
		);
		mn::str_free(self->absolute_path);
		mn::str_free(self->filepath);
		mn::str_free(self->content);
		destruct(self->errs);
		mn::buf_free(self->tkns);
		mn::buf_free(self->lines);
		mn::allocator_free(self->ast_arena);
		mn::buf_free(self->decls);
		scope_free(self->file_scope);
		mn::free(self);
	}

	bool
	unit_file_scan(Unit_File* self)
	{
		auto scanner = scanner_new(self);
		while (true)
		{
			auto tkn = scanner_scan(scanner);
			if (tkn.kind == Tkn::KIND_EOF)
				break;

			if (tkn.kind != Tkn::KIND_NONE)
			{
				mn::buf_push(self->tkns, tkn);
			}
		}
		return self->errs.count == 0;
	}

	bool
	unit_file_parse(Unit_File* self)
	{
		auto parser = parser_new(self);
		mn_defer(parser_free(parser));

		self->package_name = parser_parse_package(parser);

		while (true)
		{
			auto decl = parser_parse_decl(parser);
			if (decl == nullptr)
				break;
			mn::buf_push(self->decls, decl);
		}

		return self->errs.count == 0;
	}

	void
	unit_file_dump_tokens(Unit_File* self, mn::Stream out)
	{
		for (auto tkn: self->tkns)
		{
			if (mn::stream_cursor_pos(out) > 0)
				mn::print_to(out, "\n");
			auto tkn_str = mn::str_from_substr(tkn.loc.rng.begin, tkn.loc.rng.end, mn::memory::tmp());
			mn::print_to(out, "line: {}, col: {}, kind: {}, str: \"{}\"", tkn.loc.pos.line, tkn.loc.pos.col, Tkn::NAMES[tkn.kind], tkn_str);
		}
	}

	void
	unit_file_dump_errors(Unit_File* self, mn::Stream out)
	{
		for (auto err: self->errs)
			err_dump(err, out);
	}

	mn::Result<Unit_Package*>
	unit_file_resolve_package(Unit_File* self, const mn::Str& path)
	{
		auto old_pwd = mn::path_current(mn::memory::tmp());
		mn_defer(mn::path_current_change(old_pwd));

		// search for package in std library
		if (self->parent_package->parent_unit->std_library_folder_path.count > 0)
		{
			auto unit = self->parent_package->parent_unit;
			mn::path_current_change(unit->std_library_folder_path);

			auto absolute_path = mn::path_absolute(path, mn::memory::tmp());
			if (mn::path_exists(absolute_path))
				return unit_package_resolve_package(self->parent_package, absolute_path);
		}

		// search for package relative to the file
		{
			auto file_dir = mn::file_directory(self->absolute_path, mn::memory::tmp());
			mn::path_current_change(file_dir);

			auto absolute_path = mn::path_absolute(path, mn::memory::tmp());

			return unit_package_resolve_package(self->parent_package, absolute_path);
		}
	}

	Unit_Package*
	unit_package_new()
	{
		auto self = mn::alloc_zerod<Unit_Package>();
		self->stage = COMPILATION_STAGE_SCAN;
		self->symbols_arena = mn::allocator_arena_new();
		return self;
	}

	void
	unit_package_free(Unit_Package* self)
	{
		mn::log_debug(
			"Package '{}': Symbols {}/{}, (used/reserved)bytes",
			self->absolute_path,
			self->symbols_arena->used_mem, self->symbols_arena->total_mem
		);
		mn::str_free(self->absolute_path);
		destruct(self->files);
		mn::map_free(self->absolute_path_to_file);
		destruct(self->errs);
		mn::buf_free(self->reachable_symbols);
		mn::allocator_free(self->symbols_arena);
		scope_free(self->global_scope);
		mn::buf_free(self->imported_packages);
		mn::free(self);
	}

	bool
	unit_package_add_file(Unit_Package* self, Unit_File* file)
	{
		if (mn::map_lookup(self->absolute_path_to_file, file->absolute_path) != nullptr)
			return false;

		file->parent_package = self;
		mn::buf_push(self->files, file);
		mn::map_insert(self->absolute_path_to_file, file->absolute_path, file);
		return true;
	}

	bool
	unit_package_scan(Unit_Package* self)
	{
		if (self->stage == COMPILATION_STAGE_SCAN)
		{
			bool has_errors = false;
			for (auto file: self->files)
				if (unit_file_scan(file) == false)
					has_errors = true;

			if (has_errors)
				self->stage = COMPILATION_STAGE_FAILED;
			else
				self->stage = COMPILATION_STAGE_PARSE;
			return has_errors == false;
		}
		else
		{
			return unit_package_has_errors(self) == false;
		}
	}

	bool
	unit_package_parse(Unit_Package* self)
	{
		if (self->stage == COMPILATION_STAGE_PARSE)
		{
			bool has_errors = false;
			Tkn package_name{};
			for (auto file: self->files)
			{
				if (unit_file_parse(file) == false)
				{
					has_errors = true;
				}
				else
				{
					if (package_name.kind == Tkn::KIND_NONE)
					{
						package_name = file->package_name;
					}
					else
					{
						if (package_name.str != file->package_name.str)
						{
							Err err{};
							err.loc = file->package_name.loc;
							err.msg = mn::strf(
								"package file has different package name than other files, package name should be '{}' but found '{}', first name was defined in {}:{}",
								package_name.str,
								file->package_name.str,
								package_name.loc.file->absolute_path,
								package_name.loc.pos.line
							);
							unit_err(file, err);
							has_errors = true;
						}
					}
				}
			}

			if (has_errors)
			{
				self->stage = COMPILATION_STAGE_FAILED;
			}
			else
			{
				self->stage = COMPILATION_STAGE_CHECK;
				self->name = package_name;
				self->global_scope = scope_new(nullptr, self->name.str, nullptr, Scope::FLAG_NONE);

				for (auto file: self->files)
					file->file_scope = scope_new(self->global_scope, "", nullptr, Scope::FLAG_NONE);
			}
			return has_errors == false;
		}
		else
		{
			return unit_package_has_errors(self) == false;
		}
	}

	bool
	unit_package_check(Unit_Package* self)
	{
		if (self->stage == COMPILATION_STAGE_CHECK)
		{
			bool has_errors = false;
			auto typer = typer_new(self);
			mn_defer(typer_free(typer));
			typer_check(typer);
			if (unit_package_has_errors(self))
			{
				has_errors = true;
				self->stage = COMPILATION_STAGE_FAILED;
			}
			else
			{
				self->stage = COMPILATION_STAGE_CODEGEN;
			}
			return has_errors == false;
		}
		else
		{
			return unit_package_has_errors(self) == false;
		}
	}

	void
	unit_package_dump_tokens(Unit_Package* self, mn::Stream out)
	{
		for (auto file: self->files)
			unit_file_dump_tokens(file, out);
	}

	void
	unit_package_dump_errors(Unit_Package* self, mn::Stream out)
	{
		for (auto file: self->files)
			unit_file_dump_errors(file, out);
		for (auto err: self->errs)
			err_dump(err, out);
	}

	mn::Result<Unit_Package*>
	unit_package_resolve_package(Unit_Package* self, const mn::Str& absolute_path)
	{
		auto [package, err] = unit_resolve_package(self->parent_unit, absolute_path);
		if (err == false)
		{
			mn::buf_push(self->imported_packages, package);
			return package;
		}
		return err;
	}

	Unit*
	unit_from_file(const mn::Str& filepath, const mn::Str& entry, const mn::Str& std_path)
	{
		auto self = mn::alloc_zerod<Unit>();

		self->std_library_folder_path = clone(std_path);
		auto root_file = unit_file_from_path(filepath);
		auto root_package = unit_package_new();
		// single file packages has their paths be the file path
		root_package->absolute_path = clone(root_file->absolute_path);
		unit_package_add_file(root_package, root_file);

		self->str_interner = mn::str_intern_new();
		self->type_interner = type_interner_new();
		self->mode = COMPILATION_MODE_LIBRARY;

		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_UNIFORM));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_BUILTIN));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_BINDING));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_VERTEX));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_PIXEL));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_SV_POSITION));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_GLSL));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_PIPELINE));

		if (entry.count > 0)
			self->entry = mn::str_intern(self->str_interner, entry.ptr);

		unit_add_package(self, root_package);

		return self;
	}

	void
	unit_free(Unit* self)
	{
		mn::log_debug(
			"Types: {}/{}, (used/reserved)bytes",
			self->type_interner.arena->used_mem, self->type_interner.arena->total_mem
		);

		mn::str_intern_free(self->str_interner);
		type_interner_free(self->type_interner);
		destruct(self->scope_table);
		destruct(self->packages);
		mn::map_free(self->absolute_path_to_package);
		mn::str_free(self->std_library_folder_path);
		mn::map_free(self->input_layout);
		mn::buf_free(self->reachable_uniforms);
		mn::free(self);
	}

	bool
	unit_scan(Unit* self)
	{
		bool has_errors = false;
		for (auto package: self->packages)
			if (unit_package_scan(package) == false)
				has_errors = true;
		return has_errors == false;
	}

	bool
	unit_parse(Unit* self)
	{
		bool has_errors = false;
		for (size_t i = 0; i < self->packages.count; ++i)
		{
			auto package = self->packages[i];

			package->state = Unit_Package::STATE_RESOLVING;
			if (unit_package_parse(package) == false)
				has_errors = true;

			for (auto imported_package: package->imported_packages)
			{
				unit_package_scan(imported_package);
				unit_package_parse(imported_package);
			}
		}
		for (auto package: self->packages)
		{
			package->state = Unit_Package::STATE_RESOLVED;
		}
		return has_errors == false;
	}

	bool
	unit_check(Unit* self)
	{
		bool has_errors = false;
		for (size_t i = 0; i < self->packages.count; ++i)
		{
			auto package = self->packages[i];
			if (unit_package_check(package) == false)
				has_errors = true;

			// if we have a unit which is not in library mode
			// then it's enough to check the first/main package only
			// we don't need to go through other packages because we
			// check only their used symbols
			if (self->mode != COMPILATION_MODE_LIBRARY)
			{
				break;
			}
		}
		return has_errors == false;
	}

	bool
	unit_reflect(Unit* self)
	{
		bool has_errors = false;
		if (self->entry_symbol)
		{
			auto package = self->entry_symbol->package;
			reflect_package(package);
		}
		return has_errors == false;
	}

	mn::Str
	unit_reflection_info_as_json(Unit* self, mn::Allocator allocator)
	{
		auto types = mn::set_with_allocator<Type*>(mn::memory::tmp());

		auto json_entry = mn::json::value_object_new();
		if (self->entry_symbol)
		{
			mn::json::value_object_insert(json_entry, "name", mn::json::value_string_new(self->entry_symbol->name));

			auto json_layout = mn::json::value_array_new();
			for (const auto& [attribute_name, attribute_type]: self->input_layout)
			{
				auto json_attribute = mn::json::value_object_new();
				mn::json::value_object_insert(json_attribute, "name", mn::json::value_string_new(attribute_name));
				mn::json::value_object_insert(json_attribute, "type", mn::json::value_string_new(mn::strf("{}", attribute_type)));
				mn::json::value_array_push(json_layout, json_attribute);

				_push_type(types, attribute_type);
			}
			mn::json::value_object_insert(json_entry, "input_layout", json_layout);
		}

		auto json_uniforms = mn::json::value_array_new();
		for (const auto& uniform: self->reachable_uniforms)
		{
			auto json_uniform = mn::json::value_object_new();
			mn::json::value_object_insert(json_uniform, "name", mn::json::value_string_new(uniform.symbol->name));
			mn::json::value_object_insert(json_uniform, "binding", mn::json::value_number_new(uniform.binding));
			mn::json::value_object_insert(json_uniform, "type", mn::json::value_string_new(mn::strf("{}", uniform.symbol->type)));
			mn::json::value_array_push(json_uniforms, json_uniform);

			_push_type(types, uniform.symbol->type);
		}

		auto json_types = mn::json::value_array_new();
		for (auto type: types)
		{
			auto json_type = mn::json::value_object_new();
			mn::json::value_object_insert(json_type, "name", mn::json::value_string_new(mn::strf("{}", type)));
			mn::json::value_object_insert(json_type, "size", mn::json::value_number_new(type->size));
			mn::json::value_object_insert(json_type, "alignment", mn::json::value_number_new(type->alignment));

			switch (type->kind)
			{
			case Type::KIND_STRUCT:
			{
				auto json_fields = mn::json::value_array_new();
				for (const auto& field: type->struct_type.fields)
				{
					auto json_field = mn::json::value_object_new();
					mn::json::value_object_insert(json_field, "name", mn::json::value_string_new(field.name.str));
					mn::json::value_object_insert(json_field, "type", mn::json::value_string_new(mn::strf("{}", field.type)));
					mn::json::value_object_insert(json_field, "offset", mn::json::value_number_new(field.offset));
					mn::json::value_array_push(json_fields, json_field);
				}
				mn::json::value_object_insert(json_type, "fields", json_fields);
				break;
			}
			default:
				break;
			}
			mn::json::value_array_push(json_types, json_type);
		}

		mn::json::Value json_pipeline{};
		if (self->pipeline)
		{
			if (self->pipeline->const_sym.value)
				json_pipeline = expr_value_to_json(self->pipeline->const_sym.value->const_value);
			else
				json_pipeline = expr_value_to_json(expr_value_zero(mn::memory::tmp(), self->pipeline->type));
		}

		auto json_result = mn::json::value_object_new();
		mn_defer(mn::json::value_free(json_result));

		mn::json::value_object_insert(json_result, "entry", json_entry);
		mn::json::value_object_insert(json_result, "uniforms", json_uniforms);
		mn::json::value_object_insert(json_result, "types", json_types);
		mn::json::value_object_insert(json_result, "pipeline", json_pipeline);

		return mn::strf(allocator, "{}", json_result);
	}

	mn::Result<mn::Str>
	unit_glsl(Unit* self, mn::Allocator allocator)
	{
		if (unit_has_errors(self))
			return mn::Err {"unit has errors"};

		auto stream = mn::memory_stream_new(allocator);
		mn_defer(mn::memory_stream_free(stream));

		auto glsl = glsl_new(self->packages[0], stream);
		mn_defer(glsl_free(glsl));

		glsl_gen(glsl);

		return mn::memory_stream_str(stream);
	}

	mn::Str
	unit_dump_tokens(Unit* self, mn::Allocator allocator)
	{
		auto out = mn::memory_stream_new(allocator);
		mn_defer(mn::memory_stream_free(out));

		for (auto package: self->packages)
			unit_package_dump_tokens(package, out);

		return mn::memory_stream_str(out);
	}

	mn::Str
	unit_dump_errors(Unit* self, mn::Allocator allocator)
	{
		auto out = mn::memory_stream_new(allocator);
		mn_defer(mn::memory_stream_free(out));

		for (auto package: self->packages)
			unit_package_dump_errors(package, out);

		return mn::memory_stream_str(out);
	}

	Scope*
	unit_create_scope_for(Unit* self, void* ptr, Scope* parent, const char* name, Type* expected_type, Scope::FLAG flags)
	{
		if (auto it = mn::map_lookup(self->scope_table, ptr))
			return it->value;
		auto new_scope = scope_new(parent, name, expected_type, flags);
		mn::map_insert(self->scope_table, ptr, new_scope);
		return new_scope;
	}

	mn::Result<Unit_Package*>
	unit_resolve_package(Unit* self, const mn::Str& absolute_path)
	{
		if (mn::path_exists(absolute_path) == false)
			return mn::Err{ "package path '{}' does not exist", absolute_path };

		if (mn::path_is_folder(absolute_path))
		{
			if (auto it = mn::map_lookup(self->absolute_path_to_package, absolute_path))
			{
				auto package = it->value;
				if (package->state == Unit_Package::STATE_RESOLVING)
				{
					return mn::Err { "cyclic import of package '{}'", absolute_path };
				}
				return package;
			}

			auto package = unit_package_new();
			package->absolute_path = clone(absolute_path);

			for (auto entry: mn::path_entries(absolute_path, mn::memory::tmp()))
			{
				if (entry.kind != mn::Path_Entry::KIND_FILE)
					continue;

				if (mn::str_suffix(entry.name, ".sabre") == false)
					continue;

				auto file_path = mn::path_join(mn::str_tmp(), absolute_path, entry.name);
				auto file = unit_file_from_path(file_path);
				unit_package_add_file(package, file);
			}
			unit_add_package(self, package);
			return package;
		}
		else
		{
			if (auto it = mn::map_lookup(self->absolute_path_to_package, absolute_path))
			{
				auto package = it->value;
				if (package->state == Unit_Package::STATE_RESOLVING)
				{
					return mn::Err { "cyclic import of package '{}'", absolute_path };
				}
				return package;
			}

			auto file = unit_file_from_path(absolute_path);
			auto package = unit_package_new();
			package->absolute_path = clone(absolute_path);
			unit_package_add_file(package, file);
			unit_add_package(self, package);
			return package;
		}
	}
}