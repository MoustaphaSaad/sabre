#pragma once

#include "sabre/Exports.h"

#include <mn/Stream.h>
#include <mn/Buf.h>

namespace sabre
{
	struct Unit_Package;
	struct Scope;
	struct Expr;
	struct Stmt;

	struct GLSL
	{
		Unit_Package* unit;

		mn::Stream out;
		size_t indent;

		mn::Buf<Scope*> scope_stack;
		// map from the reserved keywords to the alternative names
		mn::Map<const char*, const char*> reserved_to_alternative;
		// maps the name of a specific AST entity to some generated name, used in compound literals
		mn::Map<void*, const char*> symbol_to_names;
		mn::Buf<mn::Str> input_names;
		mn::Buf<mn::Str> output_names;
		size_t tmp_id;
	};

	// creates a new GLSL generator instance
	SABRE_EXPORT GLSL
	glsl_new(Unit_Package* unit, mn::Stream out);

	// frees the given GLSL generator
	SABRE_EXPORT void
	glsl_free(GLSL& self);

	// it will generate the GLSL code for the given expression
	SABRE_EXPORT void
	glsl_expr_gen(GLSL& self, Expr* e);

	// it will generate the GLSL code for the given statement
	SABRE_EXPORT void
	glsl_stmt_gen(GLSL& self, Stmt* s);

	// it will generate all of the reachable symbols in the given unit
	SABRE_EXPORT void
	glsl_gen(GLSL& self);
}