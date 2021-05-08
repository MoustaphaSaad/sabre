#pragma once

#include "sabre/Exports.h"

#include <mn/Stream.h>
#include <mn/Buf.h>

namespace sabre
{
	struct Unit;
	struct Scope;
	struct Expr;
	struct Stmt;

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

	// it will generate the GLSL code for the given statement
	SABRE_EXPORT void
	glsl_stmt_gen(GLSL& self, Stmt* s);
}