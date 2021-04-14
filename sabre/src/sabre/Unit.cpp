#include "sabre/Unit.h"
#include "sabre/Scan.h"

#include <mn/Path.h>
#include <mn/IO.h>
#include <mn/Log.h>

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
		return self;
	}

	void
	unit_free(Unit* self)
	{
		mn::log_debug("AST memory(used/reserved): {}/{} bytes for compilation unit '{}'", self->ast_arena->used_mem, self->ast_arena->total_mem, self->filepath);

		mn::str_free(self->filepath);
		mn::str_free(self->content);
		mn::buf_free(self->lines);
		mn::str_intern_free(self->str_interner);
		destruct(self->errs);
		mn::buf_free(self->tkns);
		mn::allocator_free(self->ast_arena);
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

	mn::Str
	unit_dump_tokens(Unit* self, mn::Allocator allocator)
	{
		auto res = mn::str_with_allocator(allocator);
		for (auto tkn: self->tkns)
		{
			if (res.count > 0)
				res = mn::strf(res, "\n");
			auto tkn_str = mn::str_from_substr(tkn.rng.begin, tkn.rng.end, mn::memory::tmp());
			res = mn::strf(res, "line: {}, col: {}, kind: {}, str: \"{}\"", tkn.pos.line, tkn.pos.col, Tkn::NAMES[tkn.kind], tkn_str);
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

			if (err.pos.line > 0)
			{
				auto l = self->lines[err.pos.line - 1];
				if (err.rng.end - err.rng.begin > 0)
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
						else if (it >= err.rng.begin && it < err.rng.end)
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
					res = mn::strf(res, "Error[{}:{}:{}]: {}", self->filepath, err.pos.line, err.pos.col, err.msg);
				}
				else
				{
					res = mn::strf(res, "Error[{}:{}:{}]: {}", self->filepath, err.pos.line, err.pos.col, err.msg);
				}
			}
			else
			{
				res = mn::strf(res, "Error[{}]: {}", self->filepath, err.msg);
			}
		}
		return res;
	}
}