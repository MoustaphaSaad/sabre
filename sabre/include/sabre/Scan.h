#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"

#include <mn/Rune.h>

namespace sabre
{
	struct Unit;

	struct Scanner
	{
		Unit* unit;
		const char* it;
		mn::Rune c;
		Pos pos;
		const char* line_begin;

		const char* keywords[Tkn::KIND_KEYWORDS__END - Tkn::KIND_KEYWORDS__BEGIN];
	};

	// creates a new scanner for this unit
	SABRE_EXPORT Scanner
	scanner_new(Unit* unit);

	// scans the next token off the unit content
	SABRE_EXPORT Tkn
	scanner_scan(Scanner& self);
}