#pragma once

#include "sabre/Exports.h"

#include <mn/Stream.h>

namespace sabre
{
	struct Unit_Package;
	struct Expr;

	enum ENTRY_IO_FLAG
	{
		ENTRY_IO_FLAG_NONE,
		ENTRY_IO_FLAG_PIXEL_OUT,
	};

	struct HLSL
	{
		Unit_Package* unit;

		mn::Stream out;
		size_t indent;

		mn::Buf<Scope*> scope_stack;
		// map from the reserved keywords to the alternative names
		mn::Map<const char*, const char*> reserved_to_alternative;
		// maps the name of a specific AST entity to some generated name, used in compound literals
		mn::Map<void*, const char*> symbol_to_names;
		// set of io structs
		mn::Map<Symbol*, ENTRY_IO_FLAG> io_structs;
		size_t tmp_id;
		// geometry shader specific data
		// holds the name of geometry shader stream variable name
		mn::Str geometry_stream_name;
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

	// generates the HLSL code for the given statement
	SABRE_EXPORT void
	hlsl_stmt_gen(HLSL& self, Stmt* s);

	// generates a single entry point's reachable symbols
	SABRE_EXPORT void
	hlsl_gen_entry(HLSL& self, Entry_Point* entry);

	// generates all of the reachable symbols in the given unit
	SABRE_EXPORT void
	hlsl_gen_library(HLSL& self);
}