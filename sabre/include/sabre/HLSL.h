#pragma once

#include "sabre/Exports.h"

#include <mn/Stream.h>

namespace sabre
{
	struct Unit_Package;
	struct Expr;
	struct Entry_Point;
	struct Scope;
	struct Symbol;
	struct Type;
	struct Stmt;

	enum ENTRY_IO_FLAG
	{
		ENTRY_IO_FLAG_NONE,
		ENTRY_IO_FLAG_PIXEL_OUT,
	};

	struct Buffer_Access_Runtime_Offset
	{
		Expr* offset;
		Type* type;
	};

	struct Buffer_Access_Info
	{
		Expr* buffer_name_expr;
		size_t compile_time_offset;
		mn::Buf<Buffer_Access_Runtime_Offset> runtime_offsets;
		size_t size;
		bool is_write;
	};

	struct HLSL
	{
		Unit_Package* unit;

		mn::Stream out;
		size_t indent;

		mn::memory::Arena* arena;

		Entry_Point* entry;
		mn::Buf<Scope*> scope_stack;
		// we use this stack to track the post statement of for loops bodies we enter
		// this is useful when we want to duplicate the loop post statement on continue
		mn::Buf<Stmt*> loop_post_stmt_stack;
		// map from the reserved keywords to the alternative names
		mn::Map<const char*, const char*> reserved_to_alternative;
		// maps the name of a specific AST entity to some generated name, used in compound literals
		mn::Map<void*, const char*> symbol_to_names;
		// set of io structs
		mn::Map<Symbol*, ENTRY_IO_FLAG> io_structs;
		size_t tmp_id;
		// geometry shader specific data
		// holds the name of geometry shader stream variable name
		const char* geometry_stream_name;
		// contains the type names of all the template names in mangled form
		mn::Map<Type*, const char*> template_mangled_names;
		// contains the offset and size info of a buffer dot expression
		mn::Map<Expr*, Buffer_Access_Info> buffer_access_info;
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

	// it will generate all of the reachable symbols in the given entry
	SABRE_EXPORT void
	hlsl_gen_entry(HLSL& self, Entry_Point* entry);

	// it will generate all of the reachable symbols in the given unit (library mode)
	SABRE_EXPORT void
	hlsl_gen_library(HLSL& self);
}