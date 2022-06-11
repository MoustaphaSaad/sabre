#include "sabre/Unit.h"
#include "sabre/Scan.h"
#include "sabre/Parse.h"
#include "sabre/Check.h"
#include "sabre/Reflect.h"
#include "sabre/GLSL.h"
#include "sabre/HLSL.h"
#include "sabre/SPIRV.h"
#include "sabre/IR_Text.h"
#include "sabre/Type_Interner.h"

#include <mn/Path.h>
#include <mn/IO.h>
#include <mn/Log.h>
#include <mn/Defer.h>
#include <mn/Json.h>
#include <mn/Ring.h>
#include <mn/Assert.h>

#include <fmt/chrono.h>

#include <chrono>

#include <algorithm>

namespace sabre
{
	inline static auto
	_capture_timepoint()
	{
		return std::chrono::time_point<std::chrono::high_resolution_clock, std::chrono::duration<double, std::milli>>(std::chrono::high_resolution_clock::now());
	}

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
		case Type::KIND_ARRAY:
			_push_type(types, t->array.base);
			break;
		default:
			break;
		}
		mn::set_insert(types, t);
	}

	inline static mn::Str
	_type_to_reflect_json(Type* t, bool is_raw)
	{
		if (t == type_void)
		{
			return mn::str_lit("void");
		}
		else if (t == type_bool)
		{
			return mn::str_lit("bool");
		}
		else if (t == type_int || t == type_lit_int)
		{
			return mn::str_lit("int");
		}
		else if (t == type_uint)
		{
			return mn::str_lit("uint");
		}
		else if (t == type_float || t == type_lit_float)
		{
			return mn::str_lit("float");
		}
		else if (t == type_double)
		{
			return mn::str_lit("double");
		}
		else if (t == type_vec2)
		{
			return mn::str_lit("vec2");
		}
		else if (t == type_vec3)
		{
			return mn::str_lit("vec3");
		}
		else if (t == type_vec4)
		{
			return mn::str_lit("vec4");
		}
		else if (t == type_bvec2)
		{
			return mn::str_lit("bvec2");
		}
		else if (t == type_bvec3)
		{
			return mn::str_lit("bvec3");
		}
		else if (t == type_bvec4)
		{
			return mn::str_lit("bvec4");
		}
		else if (t == type_ivec2)
		{
			return mn::str_lit("ivec2");
		}
		else if (t == type_ivec3)
		{
			return mn::str_lit("ivec3");
		}
		else if (t == type_ivec4)
		{
			return mn::str_lit("ivec4");
		}
		else if (t == type_uvec2)
		{
			return mn::str_lit("uvec2");
		}
		else if (t == type_uvec3)
		{
			return mn::str_lit("uvec3");
		}
		else if (t == type_uvec4)
		{
			return mn::str_lit("uvec4");
		}
		else if (t == type_dvec2)
		{
			return mn::str_lit("dvec2");
		}
		else if (t == type_dvec3)
		{
			return mn::str_lit("dvec3");
		}
		else if (t == type_dvec4)
		{
			return mn::str_lit("dvec4");
		}
		else if (t == type_mat2)
		{
			return mn::str_lit("mat2");
		}
		else if (t == type_mat3)
		{
			return mn::str_lit("mat3");
		}
		else if (t == type_mat4)
		{
			return mn::str_lit("mat4");
		}
		else if (t->kind == Type::KIND_FUNC)
		{
			auto name = mn::str_tmp();
			name = mn::strf(name, "func(");
			for (size_t i = 0; i < t->as_func.sign.args.types.count; ++i)
			{
				if (i > 0)
					name = mn::strf(name, ", ");
				name = mn::strf(name, ":{}", *(t->as_func.sign.args.types[i]));
			}
			name = mn::strf(name, "):{}", *t->as_func.sign.return_type);
			return name;
		}
		else if (t->kind == Type::KIND_STRUCT)
		{
			if (is_raw == false)
				return mn::str_tmpf("struct {}.{}", t->struct_type.symbol->package->name.str, t->struct_type.symbol->name);
			else
				return mn::str_lit(t->struct_type.symbol->name);
		}
		else if (t->kind == Type::KIND_PACKAGE)
		{
			return mn::str_tmpf("package '{}'", t->package_type.package->absolute_path);
		}
		else if (t->kind == Type::KIND_FUNC_OVERLOAD_SET)
		{
			auto name = mn::str_tmp();
			size_t overload_i = 0;
			for (auto [_, overload]: t->func_overload_set_type.overloads)
			{
				if (overload_i > 0)
					name = mn::strf(name, "\n");
				name = mn::strf(name, "{}. func(", overload_i++);
				for (size_t i = 0; i < overload->type->as_func.sign.args.types.count; ++i)
				{
					if (i > 0)
						name = mn::strf(name, ", ");
					name = mn::strf(name, ":{}", *overload->type->as_func.sign.args.types[i]);
				}
				name = mn::strf(name, "):{}", *overload->type->as_func.sign.return_type);
			}
			return name;
		}
		else if (t->kind == Type::KIND_TEXTURE)
		{
			switch (t->texture.type)
			{
			case TEXTURE_TYPE_1D:
				return mn::str_lit("Texture1D");
			case TEXTURE_TYPE_2D:
				return mn::str_lit("Texture2D");
			case TEXTURE_TYPE_3D:
				return mn::str_lit("Texture3D");
			case TEXTURE_TYPE_CUBE:
				return mn::str_lit("TextureCube");
			default:
				mn_unreachable();
				return mn::str_lit("<UNKNOWN TYPE>");
			}
		}
		else if (t->kind == Type::KIND_ARRAY)
		{
			mn::Str name{};
			auto base = _type_to_reflect_json(t->array.base, is_raw);
			if (t->array.count == -1)
			{
				name = mn::str_tmpf("[]{}", base);
			}
			else
			{
				name = mn::str_tmpf("[{}]{}", t->array.count, base);
			}
			return name;
		}
		else if (t->kind == Type::KIND_ENUM)
		{
			if (is_raw == false)
				return mn::str_tmpf("enum {}.{}", t->enum_type.symbol->package->name.str, t->enum_type.symbol->name);
			else
				return mn::str_lit(t->enum_type.symbol->name);
		}
		else
		{
			mn_unreachable();
			return mn::str_lit("<UNKNOWN TYPE>");
		}
	}

	inline static mn::Str
	_type_kind(Type* t)
	{
		if (t == type_void ||
			t == type_bool ||
			t == type_int ||
			t == type_lit_int ||
			t == type_uint ||
			t == type_float ||
			t == type_lit_float ||
			t == type_double ||
			t == type_vec2 ||
			t == type_vec3 ||
			t == type_vec4 ||
			t == type_bvec2 ||
			t == type_bvec3 ||
			t == type_bvec4 ||
			t == type_ivec2 ||
			t == type_ivec3 ||
			t == type_ivec4 ||
			t == type_uvec2 ||
			t == type_uvec3 ||
			t == type_uvec4 ||
			t == type_dvec2 ||
			t == type_dvec3 ||
			t == type_dvec4 ||
			t == type_mat2 ||
			t == type_mat3 ||
			t == type_mat4 ||
			t->kind == Type::KIND_TEXTURE)
		{
			return mn::str_lit("builtin");
		}
		else if (t->kind == Type::KIND_ARRAY)
		{
			return mn::str_lit("array");
		}
		else if (t->kind == Type::KIND_FUNC ||
				 t->kind == Type::KIND_FUNC_OVERLOAD_SET)
		{
			return mn::str_lit("func");
		}
		else if (t->kind == Type::KIND_STRUCT)
		{
			return mn::str_lit("struct");
		}
		else if (t->kind == Type::KIND_PACKAGE)
		{
			return mn::str_lit("package");
		}
		else if (t->kind == Type::KIND_ENUM)
		{
			return mn::str_lit("enum");
		}
		else
		{
			mn_unreachable();
			return mn::str_lit("<UNKNOWN TYPE>");
		}
	}

	inline static mn::json::Value
	_tags_to_json(Tag_Table& self)
	{
		auto json_tags = mn::json::value_object_new();
		for (const auto& [tag_name, tag_args]: self.table)
		{
			auto json_tag = mn::json::value_object_new();
			for (const auto& [arg_name, arg_value]: tag_args.args)
			{
				mn::json::Value value{};
				if (arg_value.value->const_value.type == type_int)
				{
					value = mn::json::value_number_new(arg_value.value->const_value.as_int);
				}
				else if (arg_value.value->const_value.type == type_lit_string)
				{
					value = mn::json::value_string_new(arg_value.value->const_value.as_string);
				}
				else
				{
					mn_unreachable();
				}
				mn::json::value_object_insert(json_tag, arg_name, value);
			}
			mn::json::value_object_insert(json_tags, tag_name, json_tag);
		}
		return json_tags;
	}

	inline static mn::json::Value
	_decl_tags_to_json(Decl* decl)
	{
		if (decl)
			return _tags_to_json(decl->tags);
		return mn::json::value_object_new();
	}

	inline static void
	_entry_point_sym_sort(Entry_Point* entry, Symbol* sym, mn::Set<Symbol*>& visited, mn::Set<Symbol*>& visiting)
	{
		if (mn::set_lookup(visited, sym) ||
			mn::set_lookup(visiting, sym))
			return;

		mn::set_insert(visiting, sym);
		for (auto d: sym->dependencies)
			_entry_point_sym_sort(entry, d, visited, visiting);

		mn::set_remove(visiting, sym);
		mn::set_insert(visited, sym);

		if (sym->is_top_level ||
			sym->kind == Symbol::KIND_FUNC ||
			sym->kind == Symbol::KIND_FUNC_OVERLOAD_SET)
		{
			mn::buf_push(entry->reachable_symbols, sym);
		}
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
		#if SABRE_LOG_METRICS
		mn::log_info(
			"File '{}': AST {}/{}, (used/reserved)bytes",
			self->absolute_path,
			self->ast_arena->used_mem, self->ast_arena->total_mem
		);
		#endif

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

		auto start = _capture_timepoint();
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
		auto end = _capture_timepoint();
		#if SABRE_LOG_METRICS
		mn::log_info("File '{}' scan time {}", self->absolute_path, end - start);
		#endif

		return self->errs.count == 0;
	}

	bool
	unit_file_parse(Unit_File* self)
	{
		auto parser = parser_new(self);
		mn_defer{parser_free(parser);};

		auto start = _capture_timepoint();
		self->package_name = parser_parse_package(parser);

		while (true)
		{
			auto decl = parser_parse_decl(parser);
			if (decl == nullptr)
				break;
			mn::buf_push(self->decls, decl);
		}
		auto end = _capture_timepoint();

		#if SABRE_LOG_METRICS
		mn::log_info("File '{}' parse time {}", self->absolute_path, end - start);
		#endif

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
		mn_defer{mn::path_current_change(old_pwd);};

		auto unit = self->parent_package->parent_unit;

		// check if path is a library collection name
		if (auto it = mn::map_lookup(unit->library_collections, path))
		{
			return unit_package_resolve_package(self->parent_package, it->value);
		}

		// handle collection paths
		if (path.count > 0)
		{
			auto colon_pos = mn::str_find(path, ':', 0);
			if (colon_pos != SIZE_MAX)
			{
				auto collection_name = mn::str_from_substr(path.ptr, path.ptr + colon_pos, mn::memory::tmp());
				if (auto it = mn::map_lookup(unit->library_collections, collection_name))
				{
					auto path_in_collection = mn::path_join(mn::str_tmp(), it->value, path.ptr + colon_pos + 1);
					return unit_package_resolve_package(self->parent_package, path_in_collection);
				}
			}
		}

		// search for package relative to the file
		{
			auto file_dir = mn::file_directory(self->absolute_path, mn::memory::tmp());
			mn::path_current_change(file_dir);

			auto absolute_path = mn::path_absolute(path, mn::memory::tmp());

			return unit_package_resolve_package(self->parent_package, absolute_path);
		}
	}

	void
	entry_point_calc_reachable_list(Entry_Point* entry)
	{
		// only calc it once
		if (entry->reachable_symbols.count > 0)
			return;

		auto visited = mn::set_with_allocator<Symbol*>(mn::memory::tmp());
		auto visiting = mn::set_with_allocator<Symbol*>(mn::memory::tmp());
		_entry_point_sym_sort(entry, entry->symbol, visited, visiting);
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
		#if SABRE_LOG_METRICS
		mn::log_info(
			"Package '{}': Symbols {}/{}, (used/reserved)bytes",
			self->absolute_path,
			self->symbols_arena->used_mem, self->symbols_arena->total_mem
		);
		#endif

		mn::str_free(self->absolute_path);
		destruct(self->files);
		mn::map_free(self->absolute_path_to_file);
		destruct(self->errs);
		destruct(self->entry_points);
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

			auto start = _capture_timepoint();
			for (auto file: self->files)
				if (unit_file_scan(file) == false)
					has_errors = true;
			auto end = _capture_timepoint();

			#if SABRE_LOG_METRICS
			mn::log_info("Package '{}' scan time: {}", self->absolute_path, end - start);
			#endif

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
			auto start = _capture_timepoint();
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

			auto end = _capture_timepoint();
			#if SABRE_LOG_METRICS
			mn::log_info("Package '{}' parse time {}", self->absolute_path, end - start);
			#endif
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
			auto start = _capture_timepoint();
			bool has_errors = false;
			auto typer = typer_new(self);
			mn_defer{typer_free(typer);};
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
			auto end = _capture_timepoint();
			#if SABRE_LOG_METRICS
			mn::log_info("Package '{}' checking time {}", self->absolute_path, end - start);
			#endif
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

	Entry_Point*
	unit_package_entry_find(Unit_Package* self, const mn::Str& name)
	{
		for (auto entry: self->entry_points)
			if (entry->symbol->name == name)
				return entry;
		return nullptr;
	}

	Unit*
	unit_from_file(const mn::Str& filepath, const mn::Str& entry, BACKEND_MODE backend)
	{
		auto self = mn::alloc_zerod<Unit>();

		self->root_file = unit_file_from_path(filepath);
		self->root_package = unit_package_new();
		// single file packages has their paths be the file path
		self->root_package->absolute_path = clone(self->root_file->absolute_path);
		unit_package_add_file(self->root_package, self->root_file);

		self->str_interner = mn::str_intern_new();
		self->type_interner = type_interner_new();
		self->backend = backend;

		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_UNIFORM));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_BUILTIN));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_BINDING));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_VERTEX));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_PIXEL));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_SV_POSITION));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_SV_DEPTH));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_SV_PRIMITIVE_ID));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_SV_THREAD_ID));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_GLSL));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_REFLECT));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_HLSL));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_HLSL_METHOD));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_SAMPLER_STATE));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_SAMPLE_FUNC));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_GEOMETRY));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_MAX_VERTEX_COUNT));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_IN));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_OUT));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_INOUT));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_POINT));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_LINE));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_TRIANGLE));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_READ_WRITE));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_COMPUTE));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_X));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_Y));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_Z));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_BUILD_BACKEND));
		mn::set_insert(self->str_interner.strings, mn::str_lit(KEYWORD_BUFFER));

		unit_add_package(self, self->root_package);

		return self;
	}

	void
	unit_free(Unit* self)
	{
		#if SABRE_LOG_METRICS
		mn::log_info(
			"Types: {}/{}, (used/reserved)bytes",
			self->type_interner->arena->used_mem, self->type_interner->arena->total_mem
		);
		#endif

		mn::str_intern_free(self->str_interner);
		type_interner_free(self->type_interner);
		destruct(self->scope_table);
		destruct(self->packages);
		mn::map_free(self->absolute_path_to_package);
		mn::map_free(self->reachable_uniforms);
		mn::map_free(self->reachable_textures);
		mn::map_free(self->reachable_samplers);
		mn::buf_free(self->reflected_symbols);
		destruct(self->library_collections);
		mn::buf_free(self->symbol_stack);
		mn::buf_free(self->all_uniforms);
		mn::free(self);
	}

	bool
	unit_scan(Unit* self)
	{
		bool has_errors = false;

		auto start = _capture_timepoint();
		for (auto package: self->packages)
			if (unit_package_scan(package) == false)
				has_errors = true;
		auto end = _capture_timepoint();

		#if SABRE_LOG_METRICS
		mn::log_info("Total scan time: {}", end - start);
		#endif
		return has_errors == false;
	}

	bool
	unit_parse(Unit* self)
	{
		bool has_errors = false;
		auto start = _capture_timepoint();
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
		auto end = _capture_timepoint();

		#if SABRE_LOG_METRICS
		mn::log_info("Total parse time: {}", end - start);
		#endif
		return has_errors == false;
	}

	bool
	unit_check(Unit* self)
	{
		bool has_errors = false;
		auto start = _capture_timepoint();
		for (size_t i = 0; i < self->packages.count; ++i)
		{
			auto package = self->packages[i];
			if (unit_package_check(package) == false)
				has_errors = true;
		}
		auto end = _capture_timepoint();
		#if SABRE_LOG_METRICS
		mn::log_info("Total checking time {}", end - start);
		#endif
		return has_errors == false;
	}

	bool
	unit_reflect(Unit* self, Entry_Point* entry)
	{
		if (entry == nullptr)
			return false;

		reflect_package(entry);
		return true;
	}

	mn::Str
	unit_reflection_info_as_json(Unit* self, Entry_Point* entry, mn::Allocator allocator)
	{
		auto types = mn::set_with_allocator<Type*>(mn::memory::tmp());

		auto json_entry = mn::json::value_object_new();
		if (entry)
		{
			mn::json::value_object_insert(json_entry, "name", mn::json::value_string_new(entry->symbol->name));

			auto json_layout = mn::json::value_array_new();
			for (const auto& [attribute_name, attribute]: entry->input_layout)
			{
				auto json_attribute = mn::json::value_object_new();
				mn::json::value_object_insert(json_attribute, "name", mn::json::value_string_new(attribute_name));
				mn::json::value_object_insert(json_attribute, "type", mn::json::value_string_new(_type_to_reflect_json(attribute.type, false)));
				if (attribute.tags)
					mn::json::value_object_insert(json_attribute, "tags", _tags_to_json(*attribute.tags));
				mn::json::value_array_push(json_layout, json_attribute);

				_push_type(types, attribute.type);
			}
			mn::json::value_object_insert(json_entry, "input_layout", json_layout);
		}

		auto json_uniforms = mn::json::value_array_new();
		for (auto symbol: entry->uniforms)
		{
			mn_assert(symbol->kind == Symbol::KIND_VAR);
			auto binding = symbol->var_sym.uniform_binding;

			auto json_uniform = mn::json::value_object_new();
			if (symbol->package == self->root_package)
			{
				mn::json::value_object_insert(json_uniform, "name", mn::json::value_string_new(symbol->name));
			}
			else
			{
				auto uniform_name = mn::json::value_string_new(mn::str_tmpf("{}.{}", symbol->package->name.str, symbol->name));
				mn::json::value_object_insert(json_uniform, "name", uniform_name);
			}
			mn::json::value_object_insert(json_uniform, "binding", mn::json::value_number_new(binding));
			mn::json::value_object_insert(json_uniform, "type", mn::json::value_string_new(_type_to_reflect_json(symbol->type, false)));
			mn::json::value_object_insert(json_uniform, "tags", _decl_tags_to_json(symbol_decl(symbol)));
			mn::json::value_object_insert(json_uniform, "size", mn::json::value_number_new(symbol->type->unaligned_size));

			mn::json::value_array_push(json_uniforms, json_uniform);

			_push_type(types, symbol->type);
		}

		auto json_textures = mn::json::value_array_new();
		for (auto symbol: entry->textures)
		{
			mn_assert(symbol->kind == Symbol::KIND_VAR);
			auto binding = symbol->var_sym.uniform_binding;

			auto json_texture = mn::json::value_object_new();
			if (symbol->package == self->root_package)
			{
				mn::json::value_object_insert(json_texture, "name", mn::json::value_string_new(symbol->name));
			}
			else
			{
				auto uniform_name = mn::json::value_string_new(mn::str_tmpf("{}.{}", symbol->package->name.str, symbol->name));
				mn::json::value_object_insert(json_texture, "name", uniform_name);
			}
			mn::json::value_object_insert(json_texture, "binding", mn::json::value_number_new(binding));
			mn::json::value_object_insert(json_texture, "type", mn::json::value_string_new(_type_to_reflect_json(symbol->type, false)));
			mn::json::value_object_insert(json_texture, "tags", _decl_tags_to_json(symbol_decl(symbol)));
			mn::json::value_array_push(json_textures, json_texture);

			_push_type(types, symbol->type);
		}

		auto json_types = mn::json::value_array_new();
		for (auto type: types)
		{
			auto json_type = mn::json::value_object_new();
			mn::json::value_object_insert(json_type, "name", mn::json::value_string_new(_type_to_reflect_json(type, false)));
			mn::json::value_object_insert(json_type, "raw_name", mn::json::value_string_new(_type_to_reflect_json(type, true)));
			mn::json::value_object_insert(json_type, "kind", mn::json::value_string_new(_type_kind(type)));
			mn::json::value_object_insert(json_type, "aligned_size", mn::json::value_number_new(_type_aligned_size(type)));
			mn::json::value_object_insert(json_type, "unaligned_size", mn::json::value_number_new(type->unaligned_size));
			mn::json::value_object_insert(json_type, "alignment", mn::json::value_number_new(type->alignment));

			mn::json::Value tags{};
			if (auto symbol = type_symbol(type))
			{
				tags = _decl_tags_to_json(symbol_decl(symbol));
			}
			else
			{
				tags = mn::json::value_object_new();
			}
			mn::json::value_object_insert(json_type, "tags", tags);

			switch (type->kind)
			{
			case Type::KIND_STRUCT:
			{
				auto json_fields = mn::json::value_array_new();
				for (const auto& field: type->struct_type.fields)
				{
					auto json_field = mn::json::value_object_new();
					mn::json::value_object_insert(json_field, "name", mn::json::value_string_new(field.name.str));
					mn::json::value_object_insert(json_field, "type", mn::json::value_string_new(_type_to_reflect_json(field.type, false)));
					mn::json::value_object_insert(json_field, "offset", mn::json::value_number_new(field.aligned_offset));
					mn::json::value_array_push(json_fields, json_field);
				}
				mn::json::value_object_insert(json_type, "fields", json_fields);
				break;
			}
			case Type::KIND_ARRAY:
			{
				mn::json::value_object_insert(json_type, "array_base_type", mn::json::value_string_new(_type_to_reflect_json(type->array.base, false)));
				mn::json::value_object_insert(json_type, "array_count", mn::json::value_number_new(type->array.count));
				mn::json::value_object_insert(json_type, "array_stride", mn::json::value_number_new(type->unaligned_size / type->array.count));
				break;
			}
			default:
				break;
			}
			mn::json::value_array_push(json_types, json_type);
		}

		auto json_result = mn::json::value_object_new();
		mn_defer{mn::json::value_free(json_result);};

		mn::json::value_object_insert(json_result, "package", mn::json::value_string_new(self->root_package->name.str));
		mn::json::value_object_insert(json_result, "entry", json_entry);
		mn::json::value_object_insert(json_result, "uniforms", json_uniforms);
		mn::json::value_object_insert(json_result, "textures", json_textures);
		mn::json::value_object_insert(json_result, "types", json_types);

		for (auto s: self->reflected_symbols)
		{
			mn::json::Value json_sym{};
			if (s->const_sym.value)
				json_sym = expr_value_to_json(s->const_sym.value->const_value);
			else
				json_sym = expr_value_to_json(expr_value_zero(mn::memory::tmp(), s->type));

			if (s->package != self->root_package)
			{
				auto sym_name = mn::str_tmpf("{}.{}", s->package->name.str, s->name);
				mn::json::value_object_insert(json_result, sym_name, json_sym);
			}
			else
			{
				mn::json::value_object_insert(json_result, s->name, json_sym);
			}
		}

		return mn::strf(allocator, "{}", json_result);
	}

	mn::Result<mn::Str>
	unit_glsl(Unit* self, Entry_Point* entry, mn::Allocator allocator)
	{
		if (entry)
		{
			auto typer = typer_new(entry->symbol->package);
			typer_check_entry(typer, entry);
			typer_free(typer);
		}

		if (unit_has_errors(self))
			return mn::Err {"unit has errors"};

		auto start = _capture_timepoint();
		auto stream = mn::memory_stream_new(allocator);
		mn_defer{mn::memory_stream_free(stream);};

		auto glsl = glsl_new(self->root_package, stream);
		mn_defer{glsl_free(glsl);};

		if (entry)
			glsl_gen_entry(glsl, entry);
		else
			glsl_gen_library(glsl);
		auto end = _capture_timepoint();

		#if SABRE_LOG_METRICS
		mn::log_info("Total GLSL gen time {}", end - start);
		#endif

		return mn::memory_stream_str(stream);
	}

	mn::Result<mn::Str>
	unit_hlsl(Unit* self, Entry_Point* entry, mn::Allocator allocator)
	{
		if (entry)
		{
			auto typer = typer_new(entry->symbol->package);
			typer_check_entry(typer, entry);
			typer_free(typer);
		}

		if (unit_has_errors(self))
			return mn::Err {"unit has errors"};

		auto start = _capture_timepoint();
		auto stream = mn::memory_stream_new(allocator);
		mn_defer{mn::memory_stream_free(stream);};

		auto hlsl = hlsl_new(self->root_package, stream);
		mn_defer{hlsl_free(hlsl);};

		if (entry)
			hlsl_gen_entry(hlsl, entry);
		else
			hlsl_gen_library(hlsl);
		auto end = _capture_timepoint();
		#if SABRE_LOG_METRICS
		mn::log_info("Total HLSL gen time {}", end - start);
		#endif

		return mn::memory_stream_str(stream);
	}

	mn::Result<mn::Str>
	unit_spirv(Unit* self, mn::Allocator allocator)
	{
		if (unit_has_errors(self))
			return mn::Err {"unit has errors"};

		auto start = _capture_timepoint();
		auto stream = mn::memory_stream_new(allocator);
		mn_defer{mn::memory_stream_free(stream);};

		auto bc = spirv_new(self->root_package);
		mn_defer{spirv_free(bc);};

		spirv_gen(bc);
		auto end = _capture_timepoint();
		#if SABRE_LOG_METRICS
		mn::log_info("Total SPIRV gen time {}", end - start);
		#endif

		auto ir_text = spirv::ir_text_new(stream, bc.out);
		mn_defer{spirv::ir_text_free(ir_text);};

		spirv::ir_text_gen(ir_text);

		return mn::memory_stream_str(stream);
	}

	mn::Str
	unit_dump_tokens(Unit* self, mn::Allocator allocator)
	{
		auto out = mn::memory_stream_new(allocator);
		mn_defer{mn::memory_stream_free(out);};

		for (auto package: self->packages)
			unit_package_dump_tokens(package, out);

		return mn::memory_stream_str(out);
	}

	mn::Str
	unit_dump_errors(Unit* self, mn::Allocator allocator)
	{
		auto out = mn::memory_stream_new(allocator);
		mn_defer{mn::memory_stream_free(out);};

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