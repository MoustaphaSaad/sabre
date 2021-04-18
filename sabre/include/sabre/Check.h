#pragma once

#include "sabre/Exports.h"

#include <mn/Buf.h>

namespace sabre
{
	struct Unit;
	struct Scope;

	// type checker state
	struct Typer
	{
		Unit* unit;
		Scope* global_scope;
		mn::Buf<Scope*> scope_stack;
	};

	// creates a new type checker
	SABRE_EXPORT Typer
	typer_new(Unit* unit);

	// frees the given type checker
	SABRE_EXPORT void
	typer_free(Typer& self);

	inline static void
	destruct(Typer& self)
	{
		typer_free(self);
	}

	// performs type checking on the initialized typer
	SABRE_EXPORT void
	typer_check(Typer& self);
}