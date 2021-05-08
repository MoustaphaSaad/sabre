#include "sabre/GLSL.h"
#include "sabre/Unit.h"
#include "sabre/Type_Interner.h"
#include "sabre/AST.h"

#include <mn/Defer.h>

namespace sabre
{
	inline static void
	_glsl_newline(GLSL& self)
	{
		mn::print_to(self.out, "\n");
		for (size_t i = 0; i < self.indent; ++i)
			mn::print_to(self.out, "\t");
	}

	inline static void
	_glsl_enter_scope(GLSL& self, Scope* scope)
	{
		mn::buf_push(self.scope_stack, scope);
	}

	inline static void
	_glsl_leave_scope(GLSL& self)
	{
		assert(self.scope_stack.count > 1);
		mn::buf_pop(self.scope_stack);
	}

	inline static void
	_glsl_write_field(GLSL& self, Type* type, const mn::Str& name)
	{
		switch (type->kind)
		{
		case Type::KIND_VOID:
			mn::print_to(self.out, "void");
			break;
		case Type::KIND_BOOL:
			mn::print_to(self.out, "bool");
			break;
		case Type::KIND_INT:
			mn::print_to(self.out, "int");
			break;
		case Type::KIND_FLOAT:
			mn::print_to(self.out, "float");
			break;
		case Type::KIND_DOUBLE:
			mn::print_to(self.out, "double");
			break;
		case Type::KIND_VEC:
			if (type->vec.base == type_bool)
			{
				switch(type->vec.width)
				{
				case 2:
					mn::print_to(self.out, "bvec2");
					break;
				case 3:
					mn::print_to(self.out, "bvec3");
					break;
				case 4:
					mn::print_to(self.out, "bvec4");
					break;
				default:
					assert(false && "unreachable");
					break;
				}
			}
			else if (type->vec.base == type_int)
			{
				switch(type->vec.width)
				{
				case 2:
					mn::print_to(self.out, "ivec2");
					break;
				case 3:
					mn::print_to(self.out, "ivec3");
					break;
				case 4:
					mn::print_to(self.out, "ivec4");
					break;
				default:
					assert(false && "unreachable");
					break;
				}
			}
			else if (type->vec.base == type_uint)
			{
				switch(type->vec.width)
				{
				case 2:
					mn::print_to(self.out, "uvec2");
					break;
				case 3:
					mn::print_to(self.out, "uvec3");
					break;
				case 4:
					mn::print_to(self.out, "uvec4");
					break;
				default:
					assert(false && "unreachable");
					break;
				}
			}
			else if (type->vec.base == type_float)
			{
				switch(type->vec.width)
				{
				case 2:
					mn::print_to(self.out, "vec2");
					break;
				case 3:
					mn::print_to(self.out, "vec3");
					break;
				case 4:
					mn::print_to(self.out, "vec4");
					break;
				default:
					assert(false && "unreachable");
					break;
				}
			}
			else if (type->vec.base == type_double)
			{
				switch(type->vec.width)
				{
				case 2:
					mn::print_to(self.out, "dvec2");
					break;
				case 3:
					mn::print_to(self.out, "dvec3");
					break;
				case 4:
					mn::print_to(self.out, "dvec4");
					break;
				default:
					assert(false && "unreachable");
					break;
				}
			}
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_glsl_write_field(GLSL& self, Type* type, const char* name)
	{
		return _glsl_write_field(self, type, mn::str_lit(name));
	}

	inline static void
	_glsl_gen_atom_expr(GLSL& self, Expr* e)
	{
		mn::print_to(self.out, e->atom.str);
	}

	inline static void
	_glsl_gen_binary_expr(GLSL& self, Expr* e)
	{
		glsl_expr_gen(self, e->binary.left);
		mn::print_to(self.out, " {} ", e->binary.op.str);
		glsl_expr_gen(self, e->binary.right);
	}

	inline static void
	_glsl_gen_unary_expr(GLSL& self, Expr* e)
	{
		mn::print_to(self.out, "{}", e->unary.op.str);
		glsl_expr_gen(self, e->unary.base);
	}

	inline static void
	_glsl_gen_dot_expr(GLSL& self, Expr* e)
	{
		glsl_expr_gen(self, e->dot.lhs);
		mn::print_to(self.out, ".");
		glsl_expr_gen(self, e->dot.rhs);
	}

	inline static void
	_glsl_gen_indexed_expr(GLSL& self, Expr* e)
	{
		glsl_expr_gen(self, e->indexed.base);
		mn::print_to(self.out, "[");
		glsl_expr_gen(self, e->indexed.index);
		mn::print_to(self.out, "]");
	}

	inline static void
	_glsl_gen_call_expr(GLSL& self, Expr* e)
	{
		glsl_expr_gen(self, e->call.base);
		mn::print_to(self.out, "(");
		for (size_t i = 0; i < e->call.args.count; ++i)
		{
			if (i > 0)
				mn::print_to(self.out, ", ");
			glsl_expr_gen(self, e->call.args[i]);
		}
		mn::print_to(self.out, ")");
	}

	inline static void
	_glsl_gen_cast_expr(GLSL& self, Expr* e)
	{
		_glsl_write_field(self, e->type, nullptr);
		mn::print_to(self.out, "(");
		glsl_expr_gen(self, e->cast.base);
		mn::print_to(self.out, ")");
	}

	inline static void
	_glsl_gen_break_stmt(GLSL& self, Stmt* s)
	{
		mn::print_to(self.out, "break");
	}

	inline static void
	_glsl_gen_continue_stmt(GLSL& self, Stmt* s)
	{
		mn::print_to(self.out, "continue");
	}

	inline static void
	_glsl_gen_return_stmt(GLSL& self, Stmt* s)
	{
		if (s->return_stmt)
		{
			mn::print_to(self.out, "return ");
			glsl_expr_gen(self, s->return_stmt);
		}
		else
		{
			mn::print_to(self.out, "return");
		}
	}

	inline static void
	_glsl_gen_block_stmt(GLSL& self, Stmt* s);

	inline static void
	_glsl_gen_if_stmt(GLSL& self, Stmt* s)
	{
		for (size_t i = 0; i < s->if_stmt.body.count; ++i)
		{
			if (i > 0)
				mn::print_to(self.out, " else ");

			mn::print_to(self.out, "if (");
			glsl_expr_gen(self, s->if_stmt.cond[i]);
			mn::print_to(self.out, ") ");
			_glsl_gen_block_stmt(self, s->if_stmt.body[i]);
		}

		if (s->if_stmt.else_body != nullptr)
		{
			mn::print_to(self.out, " else ");
			_glsl_gen_block_stmt(self, s->if_stmt.else_body);
		}
	}

	inline static void
	_glsl_gen_for_stmt(GLSL& self, Stmt* s)
	{
		_glsl_enter_scope(self, unit_scope_find(self.unit, s));
		mn_defer(_glsl_leave_scope(self));

		mn::print_to(self.out, "{{ // for scope");
		++self.indent;

		_glsl_newline(self);
		mn::print_to(self.out, "// for init statement");
		if (s->for_stmt.init != nullptr)
		{
			_glsl_newline(self);
			glsl_stmt_gen(self, s->for_stmt.init);
			mn::print_to(self.out, ";");
		}

		_glsl_newline(self);
		mn::print_to(self.out, "while (");
		if (s->for_stmt.cond != nullptr)
			glsl_expr_gen(self, s->for_stmt.cond);
		else
			mn::print_to(self.out, "true");
		mn::print_to(self.out, ") {{");
		++self.indent;

		_glsl_newline(self);
		mn::print_to(self.out, "// for body");
		for (auto stmt: s->for_stmt.body->block_stmt)
		{
			_glsl_newline(self);
			glsl_stmt_gen(self, stmt);
			mn::print_to(self.out, ";");
		}

		if (s->for_stmt.post != nullptr)
		{
			_glsl_newline(self);
			mn::print_to(self.out, "// for post statement");
			_glsl_newline(self);
			glsl_stmt_gen(self, s->for_stmt.post);
			mn::print_to(self.out, ";");
		}

		--self.indent;
		_glsl_newline(self);
		mn::print_to(self.out, "}}");

		--self.indent;
		_glsl_newline(self);
		mn::print_to(self.out, "}} // for scope");
	}

	inline static void
	_glsl_gen_block_stmt(GLSL& self, Stmt* s)
	{
		mn::print_to(self.out, "{{");
		++self.indent;

		for (auto stmt: s->block_stmt)
		{
			_glsl_newline(self);
			glsl_stmt_gen(self, stmt);
			if (stmt->kind == Stmt::KIND_BREAK ||
				stmt->kind == Stmt::KIND_CONTINUE ||
				stmt->kind == Stmt::KIND_RETURN ||
				stmt->kind == Stmt::KIND_ASSIGN ||
				stmt->kind == Stmt::KIND_EXPR ||
				(stmt->kind == Stmt::KIND_DECL && stmt->decl_stmt->kind == Decl::KIND_VAR))
			{
				mn::print_to(self.out, ";");
			}
		}

		--self.indent;
		_glsl_newline(self);
		mn::print_to(self.out, "}}");
	}


	// API
	GLSL
	glsl_new(Unit* unit, mn::Stream out)
	{
		GLSL self{};
		self.unit = unit;
		self.out = out;
		self.indent = 0;
		self.scope_stack = mn::buf_new<Scope*>();

		// push global scope as first entry in scope stack
		auto global_scope = unit_scope_find(self.unit, nullptr);
		assert(global_scope != nullptr);
		mn::buf_push(self.scope_stack, global_scope);

		return self;
	}

	void
	glsl_free(GLSL& self)
	{
		mn::buf_free(self.scope_stack);
	}

	void
	glsl_expr_gen(GLSL& self, Expr* e)
	{
		switch (e->kind)
		{
		case Expr::KIND_ATOM:
			_glsl_gen_atom_expr(self, e);
			break;
		case Expr::KIND_BINARY:
			_glsl_gen_binary_expr(self, e);
			break;
		case Expr::KIND_UNARY:
			_glsl_gen_unary_expr(self, e);
			break;
		case Expr::KIND_DOT:
			_glsl_gen_dot_expr(self, e);
			break;
		case Expr::KIND_INDEXED:
			_glsl_gen_indexed_expr(self, e);
			break;
		case Expr::KIND_CALL:
			_glsl_gen_call_expr(self, e);
			break;
		case Expr::KIND_CAST:
			_glsl_gen_cast_expr(self, e);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	void
	glsl_stmt_gen(GLSL& self, Stmt* s)
	{
		switch (s->kind)
		{
		case Stmt::KIND_BREAK:
			_glsl_gen_break_stmt(self, s);
			break;
		case Stmt::KIND_CONTINUE:
			_glsl_gen_continue_stmt(self, s);
			break;
		case Stmt::KIND_RETURN:
			_glsl_gen_return_stmt(self, s);
			break;
		case Stmt::KIND_IF:
			_glsl_gen_if_stmt(self, s);
			break;
		case Stmt::KIND_FOR:
			_glsl_gen_for_stmt(self, s);
			break;
		case Stmt::KIND_ASSIGN:
			break;
		case Stmt::KIND_EXPR:
			break;
		case Stmt::KIND_BLOCK:
			_glsl_gen_block_stmt(self, s);
			break;
		case Stmt::KIND_DECL:
			// do nothing
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}
}