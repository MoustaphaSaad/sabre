#pragma once

#include "sabre/Exports.h"

#include <mn/Buf.h>

namespace sabre
{
	struct Unit_Package;
	struct Scope;
	struct Type;

	// type checker state
	struct Typer
	{
		Unit_Package* unit;
		Entry_Point* entry;
		mn::Buf<Symbol*> reachable_symbols;
		Scope* global_scope;
		mn::Buf<Scope*> scope_stack;
		mn::Buf<Decl*> func_stack;
		mn::Buf<Type*> expected_expr_type;
		int uniform_binding_generator;
		int texture_binding_generator;
		int sampler_binding_generator;
	};

	// creates a new type checker
	SABRE_EXPORT Typer*
	typer_new(Unit_Package* unit);

	// frees the given type checker
	SABRE_EXPORT void
	typer_free(Typer* self);

	inline static void
	destruct(Typer* self)
	{
		typer_free(self);
	}

	// performs shallow walk over all the symbols
	SABRE_EXPORT void
	typer_shallow_walk(Typer* self);

	// checks a single entry point
	SABRE_EXPORT void
	typer_check_entry(Typer* self, Entry_Point* entry);

	// performs type checking on the entire code (library mode)
	SABRE_EXPORT void
	typer_check_library(Typer* self);
}