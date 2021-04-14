#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"
#include "sabre/Err.h"

#include <mn/Str.h>
#include <mn/Buf.h>
#include <mn/Str_Intern.h>

namespace sabre
{
	struct Unit
	{
		// path of the given unit
		mn::Str filepath;
		mn::Str content;
		// line ranges
		mn::Buf<Rng> lines;
		// used to intern strings, usually token strings
		mn::Str_Intern str_interner;
		// errors emitted in this unit of compilation
		mn::Buf<Err> errs;
		// tokens scanned in this unit
		mn::Buf<Tkn> tkns;
		// all the AST values are allocated from this arena, so we don't need to manage
		// memory for AST on a node by node basis
		mn::memory::Arena* ast_arena;
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

	// scans the given unit and returns whether it finished correctly
	SABRE_EXPORT bool
	unit_scan(Unit* self);

	// dumps all the scanned tokens to a string
	SABRE_EXPORT mn::Str
	unit_dump_tokens(Unit* self, mn::Allocator allocator = mn::allocator_top());

	// dumps all the errors to a string
	SABRE_EXPORT mn::Str
	unit_dump_errors(Unit* self, mn::Allocator allocator = mn::allocator_top());

	inline static const char*
	unit_intern(Unit* self, const char* begin_it, const char* end_it)
	{
		return mn::str_intern(self->str_interner, begin_it, end_it);
	}

	inline static const char*
	unit_intern(Unit* self, const char* str)
	{
		return mn::str_intern(self->str_interner, str);
	}

	inline static void
	unit_err(Unit* self, Err err)
	{
		mn::buf_push(self->errs, err);
	}
}