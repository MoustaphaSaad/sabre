#pragma once

#include "sabre/Exports.h"

#include <mn/Memory_Stream.h>

namespace sabre
{
	struct Expr;

	struct AST_Printer
	{
		mn::Memory_Stream out;
		size_t indent_level;
	};

	// creates a new AST printer instance
	SABRE_EXPORT AST_Printer
	ast_printer_new();

	// frees the given AST printer instance
	SABRE_EXPORT void
	ast_printer_free(AST_Printer& self);

	inline static void
	destruct(AST_Printer& self)
	{
		ast_printer_free(self);
	}

	// prints the given expression
	SABRE_EXPORT void
	ast_printer_print_expr(AST_Printer& self, Expr* expr);

	inline static mn::Str
	ast_printer_str(AST_Printer& self)
	{
		return mn::memory_stream_str(self.out);
	}
}