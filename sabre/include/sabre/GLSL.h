#pragma once

#include "sabre/Exports.h"

#include <mn/Stream.h>

namespace sabre
{
	struct Unit;
	struct Scope;
	struct Expr;

	struct GLSL
	{
		Unit* unit;

		mn::Stream out;
		size_t indent;

		mn::Buf<Scope*> scope_stack;
	};

	// creates a new GLSL generator instance
	SABRE_EXPORT GLSL
	glsl_new(Unit* unit, mn::Stream out);

	// frees the given GLSL generator
	SABRE_EXPORT void
	glsl_free(GLSL& self);

	// it will generate the GLSL code for the given expression
	SABRE_EXPORT void
	glsl_expr_gen(GLSL& self, Expr* e);
}