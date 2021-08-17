#pragma once

#include "sabre/Exports.h"

#include <mn/Stream.h>

namespace sabre
{
	struct Unit_Package;
	struct Expr;

	struct HLSL
	{
		Unit_Package* unit;

		mn::Stream out;
		size_t indent;

		// map from the reserved keywords to the alternative names
		mn::Map<const char*, const char*> reserved_to_alternative;
		// maps the name of a specific AST entity to some generated name, used in compound literals
		mn::Map<void*, const char*> symbol_to_names;
	};

	// creates a new HLSL generator instance
	SABRE_EXPORT HLSL
	hlsl_new(Unit_Package* unit, mn::Stream out);

	// frees the given HLSL generator
	SABRE_EXPORT void
	hlsl_free(HLSL& self);

	// generates the HLSL code for the given expression
	SABRE_EXPORT void
	hlsl_expr_gen(HLSL& self, Expr* e);
}