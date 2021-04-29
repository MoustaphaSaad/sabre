#include "sabre/Err.h"
#include "sabre/Unit.h"

#include <mn/IO.h>

namespace sabre
{
	void
	err_dump(const Err& err, mn::Stream out)
	{
		if (mn::stream_cursor_pos(out) > 0)
			mn::print_to(out, "\n");

		if (err.loc.pos.line > 0)
		{
			auto l = err.loc.unit->lines[err.loc.pos.line - 1];
			if (err.loc.rng.end - err.loc.rng.begin > 0)
			{
				auto line_str = mn::str_from_substr(l.begin, l.end, mn::memory::tmp());
				mn::print_to(out, ">> {}\n", line_str);
				mn::print_to(out, ">> ");
				for (auto it = l.begin; it < l.end; it = mn::rune_next(it))
				{
					auto c = mn::rune_read(it);
					if (c == '\r' || c == '\r')
					{
						// do nothing
					}
					else if (it >= err.loc.rng.begin && it < err.loc.rng.end)
					{
						mn::print_to(out, "^");
					}
					else if (c == '\t')
					{
						mn::print_to(out, "\t");
					}
					else
					{
						mn::print_to(out, " ");
					}
				}
				mn::print_to(out, "\n");
				mn::print_to(out, "Error[{}:{}:{}]: {}", err.loc.unit->filepath, err.loc.pos.line, err.loc.pos.col, err.msg);
			}
			else
			{
				mn::print_to(out, "Error[{}:{}:{}]: {}", err.loc.unit->filepath, err.loc.pos.line, err.loc.pos.col, err.msg);
			}
		}
		else
		{
			mn::print_to(out, "Error[{}]: {}", err.loc.unit->filepath, err.msg);
		}
	}
}