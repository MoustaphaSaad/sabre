#include "sabre/Unit.h"
#include "sabre/Scan.h"
#include "sabre/Parse.h"
#include "sabre/Check.h"

#include <mn/Path.h>
#include <mn/IO.h>
#include <mn/Log.h>
#include <mn/Defer.h>

namespace sabre
{
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
	unit_file_resolve_package(Unit_File* self, const mn::Str& path, Tkn name)
	{
		auto old_pwd = mn::path_current(mn::memory::tmp());
		mn_defer(mn::path_current_change(old_pwd));

		auto file_dir = mn::file_directory(self->absolute_path, mn::memory::tmp());
		mn::path_current_change(file_dir);

		auto absolute_path = mn::path_absolute(path, mn::memory::tmp());

		return unit_package_resolve_package(self->parent_package, absolute_path, name);
	}

	Unit_Package*
	unit_package_new(const char* name)
	{
		auto self = mn::alloc_zerod<Unit_Package>();
		self->stage = COMPILATION_STAGE_SCAN;
		self->symbols_arena = mn::allocator_arena_new();
		self->global_scope = scope_new(nullptr, name, nullptr, Scope::FLAG_NONE);
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
		mn::map_free(self->imported_packages);
		mn::free(self);
	}

	bool
	unit_package_scan(Unit_Package* self)
	{
		bool has_errors = false;
		if (self->stage == COMPILATION_STAGE_SCAN)
		{
			for (auto file: self->files)
				if (unit_file_scan(file) == false)
					has_errors = true;

			if (has_errors)
				self->stage = COMPILATION_STAGE_FAILED;
			else
				self->stage = COMPILATION_STAGE_PARSE;
		}
		return has_errors == false;
	}

	bool
	unit_package_parse(Unit_Package* self)
	{
		bool has_errors = false;
		if (self->stage == COMPILATION_STAGE_PARSE)
		{
			for (auto file: self->files)
				if (unit_file_parse(file) == false)
					has_errors = true;

			if (has_errors)
				self->stage = COMPILATION_STAGE_FAILED;
			else
				self->stage = COMPILATION_STAGE_CHECK;
		}
		return has_errors == false;
	}

	bool
	unit_package_check(Unit_Package* self)
	{
		bool has_errors = false;
		if (self->stage == COMPILATION_STAGE_CHECK)
		{
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
		}
		return has_errors == false;
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
	unit_package_resolve_package(Unit_Package* self, const mn::Str& absolute_path, Tkn name)
	{
		auto [package, err] = unit_resolve_package(self->parent_unit, absolute_path, name.str);
		if (err == false)
		{
			mn::map_insert(self->imported_packages, name.str, package);
			return package;
		}
		return err;
	}

	Unit*
	unit_from_file(const mn::Str& filepath, const mn::Str& entry)
	{
		auto self = mn::alloc_zerod<Unit>();

		auto root_file = unit_file_from_path(filepath);
		auto root_package = unit_package_new("main");
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

			for (auto [_, imported_package]: package->imported_packages)
			{
				unit_package_scan(imported_package);
				unit_package_parse(imported_package);
			}
		}
		for (auto package: self->packages)
			package->state = Unit_Package::STATE_RESOLVED;
		return has_errors == false;
	}

	bool
	unit_check(Unit* self)
	{
		bool has_errors = false;
		for (auto package: self->packages)
			if (unit_package_check(package) == false)
				has_errors = true;
		return has_errors == false;
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
	unit_resolve_package(Unit* self, const mn::Str& absolute_path, const char* name)
	{
		if (mn::path_exists(absolute_path) == false)
			return mn::Err{ "package path '{}' does not exist", absolute_path };

		if (mn::path_is_folder(absolute_path))
		{
			return mn::Err { "folder packages are not supported" };
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
			auto package = unit_package_new(name);
			package->absolute_path = clone(absolute_path);
			unit_package_add_file(package, file);
			unit_add_package(self, package);
			return package;
		}
	}
}