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
	Unit*
	unit_from_file(const mn::Str& filepath)
	{
		auto self = mn::alloc_zerod<Unit>();
		self->filepath = clone(filepath);
		self->content = mn::file_content_str(filepath);
		self->str_interner = mn::str_intern_new();
		self->ast_arena = mn::allocator_arena_new();
		self->type_interner = type_interner_new();
		self->symbols_arena = mn::allocator_arena_new();
		self->global_scope = scope_new(nullptr, "global", nullptr, Scope::FLAG_NONE);

		mn::map_insert(self->scope_table, (void*)nullptr, self->global_scope);
		return self;
	}

	void
	unit_free(Unit* self)
	{
		mn::log_debug(
			"AST: {}/{}, Symbols: {}/{}, Types: {}/{}, (used/reserved)bytes for compilation unit '{}'",
			self->ast_arena->used_mem, self->ast_arena->total_mem,
			self->symbols_arena->used_mem, self->symbols_arena->total_mem,
			self->type_interner.arena->used_mem, self->type_interner.arena->total_mem,
			self->filepath
		);

		mn::str_free(self->filepath);
		mn::str_free(self->content);
		mn::buf_free(self->lines);
		mn::str_intern_free(self->str_interner);
		destruct(self->errs);
		mn::buf_free(self->tkns);
		mn::allocator_free(self->ast_arena);
		mn::buf_free(self->decls);
		type_interner_free(self->type_interner);
		mn::allocator_free(self->symbols_arena);
		destruct(self->scope_table);
		mn::buf_free(self->reachable_symbols);
		mn::free(self);
	}

	bool
	unit_scan(Unit* self)
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
	unit_parse(Unit* self)
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

	bool
	unit_check(Unit* self)
	{
		auto typer = typer_new(self);
		mn_defer(typer_free(typer));
		typer_check(typer);
		return self->errs.count == 0;
	}

	mn::Str
	unit_dump_tokens(Unit* self, mn::Allocator allocator)
	{
		auto res = mn::str_with_allocator(allocator);
		for (auto tkn: self->tkns)
		{
			if (res.count > 0)
				res = mn::strf(res, "\n");
			auto tkn_str = mn::str_from_substr(tkn.loc.rng.begin, tkn.loc.rng.end, mn::memory::tmp());
			res = mn::strf(res, "line: {}, col: {}, kind: {}, str: \"{}\"", tkn.loc.pos.line, tkn.loc.pos.col, Tkn::NAMES[tkn.kind], tkn_str);
		}
		return res;
	}

	mn::Str
	unit_dump_errors(Unit* self, mn::Allocator allocator)
	{
		auto res = mn::str_with_allocator(allocator);
		for (auto err: self->errs)
		{
			if (res.count > 0)
				res = mn::strf(res, "\n");

			if (err.loc.pos.line > 0)
			{
				auto l = self->lines[err.loc.pos.line - 1];
				if (err.loc.rng.end - err.loc.rng.begin > 0)
				{
					auto line_str = mn::str_from_substr(l.begin, l.end, mn::memory::tmp());
					res = mn::strf(res, ">> {}\n", line_str);
					res = mn::strf(res, ">> ");
					for (auto it = l.begin; it < l.end; it = mn::rune_next(it))
					{
						auto c = mn::rune_read(it);
						if (c == '\r' || c == '\r')
						{
							// do nothing
						}
						else if (it >= err.loc.rng.begin && it < err.loc.rng.end)
						{
							mn::str_push(res, '^');
						}
						else if (c == '\t')
						{
							mn::str_push(res, '\t');
						}
						else
						{
							mn::str_push(res, ' ');
						}
					}
					mn::str_push(res, '\n');
					res = mn::strf(res, "Error[{}:{}:{}]: {}", self->filepath, err.loc.pos.line, err.loc.pos.col, err.msg);
				}
				else
				{
					res = mn::strf(res, "Error[{}:{}:{}]: {}", self->filepath, err.loc.pos.line, err.loc.pos.col, err.msg);
				}
			}
			else
			{
				res = mn::strf(res, "Error[{}]: {}", self->filepath, err.msg);
			}
		}
		return res;
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