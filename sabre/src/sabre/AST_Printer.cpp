#include "sabre/AST_Printer.h"
#include "sabre/AST.h"

#include <mn/IO.h>

#include <assert.h>

namespace sabre
{
	inline static void
	_ast_printer_newline(AST_Printer& self)
	{
		mn::print_to(self.out, "\n");
		for (size_t i = 0; i < self.indent_level; ++i)
			mn::print_to(self.out, "  ");
	}

	inline static void
	_ast_printer_enter_scope(AST_Printer& self)
	{
		++self.indent_level;
	}

	inline static void
	_ast_printer_leave_scope(AST_Printer& self)
	{
		--self.indent_level;
	}

	inline static void
	_ast_printer_print_type(AST_Printer& self, const Type_Sign& type)
	{
		mn::print_to(self.out, "(type-sign ");
		for (auto atom: type.atoms)
		{
			switch (atom.kind)
			{
			case Type_Sign_Atom::KIND_NAMED:
				mn::print_to(self.out, "{}", atom.named.str);
				break;
			default:
				assert(false && "unsupported type sign atom type");
				break;
			}
		}
		mn::print_to(self.out, ")");
	}

	// API
	AST_Printer
	ast_printer_new()
	{
		AST_Printer self{};
		self.out = mn::memory_stream_new();
		return self;
	}

	void
	ast_printer_free(AST_Printer& self)
	{
		mn::memory_stream_free(self.out);
	}

	void
	ast_printer_print_expr(AST_Printer& self, Expr* expr)
	{
		switch(expr->kind)
		{
		case Expr::KIND_ATOM:
			mn::print_to(self.out, "(atom '{}')", expr->atom.str);
			break;
		case Expr::KIND_BINARY:
			mn::print_to(self.out, "(binary '{}'", expr->binary.op.str);
			_ast_printer_enter_scope(self);
			{
				_ast_printer_newline(self);
				ast_printer_print_expr(self, expr->binary.left);
				_ast_printer_newline(self);
				ast_printer_print_expr(self, expr->binary.right);
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Expr::KIND_UNARY:
			mn::print_to(self.out, "(unary '{}'", expr->unary.op.str);
			_ast_printer_enter_scope(self);
			{
				_ast_printer_newline(self);
				ast_printer_print_expr(self, expr->unary.base);
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Expr::KIND_CALL:
			mn::print_to(self.out, "(call ");
			ast_printer_print_expr(self, expr->call.base);
			_ast_printer_enter_scope(self);
			{
				for (auto e: expr->call.args)
				{
					_ast_printer_newline(self);
					ast_printer_print_expr(self, e);
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Expr::KIND_CAST:
			mn::print_to(self.out, "(cast");
			_ast_printer_enter_scope(self);
			{
				_ast_printer_newline(self);
				ast_printer_print_expr(self, expr->cast.base);
				_ast_printer_newline(self);
				_ast_printer_print_type(self, expr->cast.type);
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Expr::KIND_INDEXED:
			mn::print_to(self.out, "(indexed");
			_ast_printer_enter_scope(self);
			{
				_ast_printer_newline(self);
				ast_printer_print_expr(self, expr->indexed.base);
				_ast_printer_newline(self);
				ast_printer_print_expr(self, expr->indexed.index);
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		default:
			assert(false && "expression type is not handled");
			break;
		}
	}
}