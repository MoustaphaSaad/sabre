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

	Unit_Package*
	unit_package_new()
	{
		auto self = mn::alloc_zerod<Unit_Package>();
		self->symbols_arena = mn::allocator_arena_new();
		self->global_scope = scope_new(nullptr, "global", nullptr, Scope::FLAG_NONE);
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
		mn::free(self);
	}

	bool
	unit_package_scan(Unit_Package* self)
	{
		bool has_errors = false;
		for (auto file: self->files)
			if (unit_file_scan(file) == false)
				has_errors = true;
		return has_errors == false;
	}

	bool
	unit_package_parse(Unit_Package* self)
	{
		bool has_errors = false;
		for (auto file: self->files)
			if (unit_file_parse(file) == false)
				has_errors = true;
		return has_errors == false;
	}

	bool
	unit_package_check(Unit_Package* self)
	{
		auto typer = typer_new(self);
		mn_defer(typer_free(typer));
		typer_check(typer);
		return self->errs.count == 0;
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

	Unit*
	unit_from_file(const mn::Str& filepath)
	{
		auto self = mn::alloc_zerod<Unit>();

		auto root_file = unit_file_from_path(filepath);
		auto root_package = unit_package_new();
		// single file packages has their paths be the file path
		root_package->absolute_path = clone(root_file->absolute_path);
		unit_package_add_file(root_package, root_file);

		self->str_interner = mn::str_intern_new();
		self->type_interner = type_interner_new();

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
		for (auto package: self->packages)
			if (unit_package_parse(package) == false)
				has_errors = true;
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
}