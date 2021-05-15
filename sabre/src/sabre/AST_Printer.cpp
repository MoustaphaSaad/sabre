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
				if (atom.named.package_name)
					mn::print_to(self.out, "{}.", atom.named.package_name.str);
				mn::print_to(self.out, "{}", atom.named.type_name.str);
				break;
			default:
				assert(false && "unsupported type sign atom type");
				break;
			}
		}
		mn::print_to(self.out, ")");
	}

	inline static void
	_ast_printer_print_tags(AST_Printer& self, const Tag_Table& tags)
	{
		for (auto [_, tag]: tags.table)
		{
			mn::print_to(self.out, "(tag '{}'", tag.name.str);
			if (tag.args.count > 0)
			{
				_ast_printer_enter_scope(self);
				{
					for (auto [key, value]: tag.args)
					{
						_ast_printer_newline(self);
						mn::print_to(self.out, "(key: '{}', value: '{}')", key.str, value.str);
					}
				}
				_ast_printer_leave_scope(self);
				_ast_printer_newline(self);
			}
			mn::print_to(self.out, ")");
		}
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
		case Expr::KIND_DOT:
			mn::print_to(self.out, "(dot");
			_ast_printer_enter_scope(self);
			{
				_ast_printer_newline(self);
				ast_printer_print_expr(self, expr->dot.lhs);
				_ast_printer_newline(self);
				ast_printer_print_expr(self, expr->dot.rhs);
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Expr::KIND_COMPLIT:
			mn::print_to(self.out, "(complit ");
			_ast_printer_print_type(self, expr->complit.type);
			_ast_printer_enter_scope(self);
			{
				for (auto field: expr->complit.fields)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(field ");
					_ast_printer_enter_scope(self);
					{
						for (auto selector: field.selector)
						{
							_ast_printer_newline(self);
							ast_printer_print_expr(self, selector);
						}
						_ast_printer_newline(self);
						ast_printer_print_expr(self, field.value);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}
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

	void
	ast_printer_print_stmt(AST_Printer& self, Stmt* stmt)
	{
		switch (stmt->kind)
		{
		case Stmt::KIND_BREAK:
			mn::print_to(self.out, "(break-stmt)");
			break;
		case Stmt::KIND_CONTINUE:
			mn::print_to(self.out, "(continue-stmt)");
			break;
		case Stmt::KIND_RETURN:
			mn::print_to(self.out, "(return-stmt");
			_ast_printer_enter_scope(self);
			{
				_ast_printer_newline(self);
				ast_printer_print_expr(self, stmt->return_stmt);
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Stmt::KIND_IF:
			mn::print_to(self.out, "(if-stmt");
			_ast_printer_enter_scope(self);
			{
				auto max_count = stmt->if_stmt.cond.count;
				if (stmt->if_stmt.body.count > max_count)
					max_count = stmt->if_stmt.body.count;

				for (size_t i = 0; i < max_count; ++i)
				{
					if (i < stmt->if_stmt.cond.count)
					{
						_ast_printer_newline(self);
						mn::print_to(self.out, "(if-cond");
						_ast_printer_enter_scope(self);
						{
							_ast_printer_newline(self);
							ast_printer_print_expr(self, stmt->if_stmt.cond[i]);
						}
						_ast_printer_leave_scope(self);
						_ast_printer_newline(self);
						mn::print_to(self.out, ")");
					}

					if (i < stmt->if_stmt.body.count)
					{
						_ast_printer_newline(self);
						mn::print_to(self.out, "(if-body");
						_ast_printer_enter_scope(self);
						{
							_ast_printer_newline(self);
							ast_printer_print_stmt(self, stmt->if_stmt.body[i]);
						}
						_ast_printer_leave_scope(self);
						_ast_printer_newline(self);
						mn::print_to(self.out, ")");
					}
				}

				if (stmt->if_stmt.else_body != nullptr)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(else-body");
					_ast_printer_enter_scope(self);
					{
						_ast_printer_newline(self);
						ast_printer_print_stmt(self, stmt->if_stmt.else_body);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Stmt::KIND_FOR:
			mn::print_to(self.out, "(for-stmt");
			_ast_printer_enter_scope(self);
			{
				if (stmt->for_stmt.init != nullptr)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(init-stmt");
					_ast_printer_enter_scope(self);
					{
						_ast_printer_newline(self);
						ast_printer_print_stmt(self, stmt->for_stmt.init);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}

				if (stmt->for_stmt.cond != nullptr)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(cond-expr");
					_ast_printer_enter_scope(self);
					{
						_ast_printer_newline(self);
						ast_printer_print_expr(self, stmt->for_stmt.cond);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}

				if (stmt->for_stmt.post != nullptr)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(post-stmt");
					_ast_printer_enter_scope(self);
					{
						_ast_printer_newline(self);
						ast_printer_print_stmt(self, stmt->for_stmt.post);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}

				if (stmt->for_stmt.body != nullptr)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(body-stmt");
					_ast_printer_enter_scope(self);
					{
						_ast_printer_newline(self);
						ast_printer_print_stmt(self, stmt->for_stmt.body);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Stmt::KIND_ASSIGN:
			mn::print_to(self.out, "(assign-stmt '{}'", stmt->assign_stmt.op.str);
			_ast_printer_enter_scope(self);
			{
				for (auto e: stmt->assign_stmt.lhs)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(LHS");
					_ast_printer_enter_scope(self);
					{
						_ast_printer_newline(self);
						ast_printer_print_expr(self, e);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}

				for (auto e: stmt->assign_stmt.rhs)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(RHS");
					_ast_printer_enter_scope(self);
					{
						_ast_printer_newline(self);
						ast_printer_print_expr(self, e);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Stmt::KIND_EXPR:
			mn::print_to(self.out, "(expr-stmt");
			_ast_printer_enter_scope(self);
			{
				_ast_printer_newline(self);
				ast_printer_print_expr(self, stmt->expr_stmt);
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Stmt::KIND_BLOCK:
			mn::print_to(self.out, "(block-stmt");
			_ast_printer_enter_scope(self);
			{
				for (auto s: stmt->block_stmt)
				{
					_ast_printer_newline(self);
					ast_printer_print_stmt(self, s);
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Stmt::KIND_DECL:
			mn::print_to(self.out, "(decl-stmt");
			_ast_printer_enter_scope(self);
			{
				_ast_printer_newline(self);
				ast_printer_print_decl(self, stmt->decl_stmt);
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		default:
			assert(false && "statement type is not handled");
			break;
		}
	}

	void
	ast_printer_print_decl(AST_Printer& self, Decl* decl)
	{
		if (decl->tags.table.count > 0)
		{
			_ast_printer_print_tags(self, decl->tags);
			_ast_printer_newline(self);
		}

		switch (decl->kind)
		{
		case Decl::KIND_CONST:
			mn::print_to(self.out, "(const-decl");
			_ast_printer_enter_scope(self);
			{
				for (auto name: decl->const_decl.names)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(const-name {})", name.str);
				}

				if (decl->const_decl.type.atoms.count > 0)
				{
					_ast_printer_newline(self);
					_ast_printer_print_type(self, decl->const_decl.type);
				}

				for (auto value: decl->const_decl.values)
				{
					_ast_printer_newline(self);
					ast_printer_print_expr(self, value);
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Decl::KIND_VAR:
			mn::print_to(self.out, "(var-decl");
			_ast_printer_enter_scope(self);
			{
				for (auto name: decl->var_decl.names)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(var-name {})", name.str);
				}

				if (decl->var_decl.type.atoms.count > 0)
				{
					_ast_printer_newline(self);
					_ast_printer_print_type(self, decl->var_decl.type);
				}

				for (auto value: decl->var_decl.values)
				{
					_ast_printer_newline(self);
					ast_printer_print_expr(self, value);
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Decl::KIND_FUNC:
			mn::print_to(self.out, "(func-decl");
			_ast_printer_enter_scope(self);
			{
				for (const auto& arg: decl->func_decl.args)
				{
					_ast_printer_newline(self);

					if (arg.tags.table.count > 0)
					{
						_ast_printer_print_tags(self, arg.tags);
						_ast_printer_newline(self);
					}

					mn::print_to(self.out, "(arg");
					_ast_printer_enter_scope(self);
					{
						for (auto name: arg.names)
						{
							_ast_printer_newline(self);
							mn::print_to(self.out, "(arg-name '{}')", name.str);
						}

						if (arg.type.atoms.count > 0)
						{
							_ast_printer_newline(self);
							_ast_printer_print_type(self, arg.type);
						}
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}

				if (decl->func_decl.return_type.atoms.count > 0)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(return-type ");
					_ast_printer_enter_scope(self);
					{
						_ast_printer_newline(self);
						_ast_printer_print_type(self, decl->func_decl.return_type);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}

				if (decl->func_decl.body != nullptr)
				{
					_ast_printer_newline(self);
					ast_printer_print_stmt(self, decl->func_decl.body);
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Decl::KIND_STRUCT:
			mn::print_to(self.out, "(struct-decl");
			_ast_printer_enter_scope(self);
			{
				for (const auto& field: decl->struct_decl.fields)
				{
					_ast_printer_newline(self);
					if (field.tags.table.count > 0)
					{
						_ast_printer_print_tags(self, field.tags);
						_ast_printer_newline(self);
					}
					mn::print_to(self.out, "(field");
					_ast_printer_enter_scope(self);
					{
						for (auto name: field.names)
						{
							_ast_printer_newline(self);
							mn::print_to(self.out, "(field-name '{}')", name.str);
						}

						if (field.type.atoms.count > 0)
						{
							_ast_printer_newline(self);
							_ast_printer_print_type(self, field.type);
						}
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		case Decl::KIND_IMPORT:
			mn::print_to(self.out, "(import ");
			if (decl->import_decl.name)
				mn::print_to(self.out, "{} ", decl->import_decl.name.str);
			mn::print_to(self.out, "\"{}\"", decl->import_decl.path.str);
			mn::print_to(self.out, ")");
			break;
		case Decl::KIND_IF:
			mn::print_to(self.out, "(if-decl");
			_ast_printer_enter_scope(self);
			{
				auto max_count = decl->if_decl.cond.count;
				if (decl->if_decl.body.count > max_count)
					max_count = decl->if_decl.body.count;

				for (size_t i = 0; i < max_count; ++i)
				{
					if (i < decl->if_decl.cond.count)
					{
						_ast_printer_newline(self);
						mn::print_to(self.out, "(if-cond");
						_ast_printer_enter_scope(self);
						{
							_ast_printer_newline(self);
							ast_printer_print_expr(self, decl->if_decl.cond[i]);
						}
						_ast_printer_leave_scope(self);
						_ast_printer_newline(self);
						mn::print_to(self.out, ")");
					}

					if (i < decl->if_decl.body.count)
					{
						_ast_printer_newline(self);
						mn::print_to(self.out, "(if-body");
						_ast_printer_enter_scope(self);
						{
							_ast_printer_newline(self);
							for (auto sub_decl: decl->if_decl.body[i])
								ast_printer_print_decl(self, sub_decl);
						}
						_ast_printer_leave_scope(self);
						_ast_printer_newline(self);
						mn::print_to(self.out, ")");
					}
				}

				if (decl->if_decl.else_body.count > 0)
				{
					_ast_printer_newline(self);
					mn::print_to(self.out, "(else-body");
					_ast_printer_enter_scope(self);
					{
						_ast_printer_newline(self);
						for (auto sub_decl: decl->if_decl.else_body)
							ast_printer_print_decl(self, sub_decl);
					}
					_ast_printer_leave_scope(self);
					_ast_printer_newline(self);
					mn::print_to(self.out, ")");
				}
			}
			_ast_printer_leave_scope(self);
			_ast_printer_newline(self);
			mn::print_to(self.out, ")");
			break;
		default:
			assert(false && "declaration type is not handled");
			break;
		}
	}
}