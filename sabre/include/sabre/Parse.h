#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"

#include <mn/Buf.h>

namespace sabre
{
	struct Unit;
	struct Expr;

	// contains the parser state
	struct Parser
	{
		Unit* unit;
		mn::Buf<Tkn> tokens;
		size_t it;
		size_t prev_it;
	};

	// creates a new parser instance
	SABRE_EXPORT Parser
	parser_new(Unit* unit);

	// frees the given parser instance
	SABRE_EXPORT void
	parser_free(Parser& self);

	inline static void
	destruct(Parser& self)
	{
		parser_free(self);
	}

	// parses an expression
	SABRE_EXPORT Expr*
	parser_parse_expr(Parser& self);
}