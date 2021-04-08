#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"

#include <mn/Str.h>
#include <mn/Buf.h>

namespace sabre
{
	struct Unit
	{
		mn::Str filepath;
		mn::Str content;
		mn::Str program;
		// line ranges
		mn::Buf<Rng> lines;
	};

	SABRE_EXPORT Unit*
	unit_from_file(const mn::Str& filepath);

	inline static Unit*
	unit_from_file(const char* filepath)
	{
		return unit_from_file(mn::str_lit(filepath));
	}

	SABRE_EXPORT void
	unit_free(Unit* self);

	inline static void
	destruct(Unit* self)
	{
		unit_free(self);
	}
}