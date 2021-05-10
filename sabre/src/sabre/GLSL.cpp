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

	inline static Scope*
	_glsl_current_scope(GLSL& self)
	{
		return mn::buf_top(self.scope_stack);
	}

	inline static Symbol*
	_glsl_find_symbol(GLSL& self, const char* name)
	{
		auto current_scope = _glsl_current_scope(self);
		return scope_find(current_scope, name);
	}

	inline static mn::Str
	_glsl_write_field(mn::Str str, Type* type, const mn::Str& name)
	{
		bool can_write_name = false;
		switch (type->kind)
		{
		case Type::KIND_VOID:
			can_write_name = true;
			str = mn::strf(str, "void");
			break;
		case Type::KIND_BOOL:
			can_write_name = true;
			str = mn::strf(str, "bool");
			break;
		case Type::KIND_INT:
			can_write_name = true;
			str = mn::strf(str, "int");
			break;
		case Type::KIND_FLOAT:
			can_write_name = true;
			str = mn::strf(str, "float");
			break;
		case Type::KIND_DOUBLE:
			can_write_name = true;
			str = mn::strf(str, "double");
			break;
		case Type::KIND_VEC:
			can_write_name = true;
			if (type->vec.base == type_bool)
			{
				switch(type->vec.width)
				{
				case 2:
					str = mn::strf(str, "bvec2");
					break;
				case 3:
					str = mn::strf(str, "bvec3");
					break;
				case 4:
					str = mn::strf(str, "bvec4");
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
					str = mn::strf(str, "ivec2");
					break;
				case 3:
					str = mn::strf(str, "ivec3");
					break;
				case 4:
					str = mn::strf(str, "ivec4");
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
					str = mn::strf(str, "uvec2");
					break;
				case 3:
					str = mn::strf(str, "uvec3");
					break;
				case 4:
					str = mn::strf(str, "uvec4");
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
					str = mn::strf(str, "vec2");
					break;
				case 3:
					str = mn::strf(str, "vec3");
					break;
				case 4:
					str = mn::strf(str, "vec4");
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
					str = mn::strf(str, "dvec2");
					break;
				case 3:
					str = mn::strf(str, "dvec3");
					break;
				case 4:
					str = mn::strf(str, "dvec4");
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

		if (can_write_name && name.count > 0)
			str = mn::strf(str, " {}", name);
		return str;
	}

	inline static mn::Str
	_glsl_write_field(Type* type, const mn::Str& name)
	{
		return _glsl_write_field(mn::str_tmp(), type, name);
	}

	inline static mn::Str
	_glsl_write_field(Type* type, const char* name)
	{
		return _glsl_write_field(type, mn::str_lit(name));
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
		mn::print_to(self.out, "{}(", _glsl_write_field(e->type, ""));
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
		_glsl_enter_scope(self, unit_scope_find(self.unit->parent_unit, s));
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
	_glsl_assign(GLSL& self, Expr* lhs, const char* op, Expr* rhs)
	{
		glsl_expr_gen(self, lhs);
		mn::print_to(self.out, " {} ", op);
		glsl_expr_gen(self, rhs);
	}

	inline static void
	_glsl_assign(GLSL& self, Symbol* lhs, const char* op, Expr* rhs)
	{
		auto e = expr_atom_new(self.unit->symbols_arena, lhs->var_sym.name);
		e->type = lhs->type;
		e->mode = ADDRESS_MODE_VARIABLE;
		_glsl_assign(self, e, op, rhs);
	}

	inline static void
	_glsl_gen_assign_stmt(GLSL& self, Stmt* s)
	{
		for (size_t i = 0; i < s->assign_stmt.lhs.count; ++i)
		{
			if (i > 0)
			{
				mn::print_to(self.out, ";");
				_glsl_newline(self);
			}

			auto lhs = s->assign_stmt.lhs[i];
			auto rhs = s->assign_stmt.rhs[i];

			_glsl_assign(self, lhs, s->assign_stmt.op.str, rhs);
		}
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

	inline static void
	_glsl_func_gen(GLSL& self, Symbol* sym)
	{
		auto d = sym->func_sym.decl;

		auto return_type = sym->type->func.return_type;
		mn::print_to(self.out, "{} {}(", _glsl_write_field(return_type, ""), sym->name);

		if (d->func_decl.body != nullptr)
			_glsl_enter_scope(self, unit_scope_find(self.unit->parent_unit, d));
		mn_defer(if (d->func_decl.body) _glsl_leave_scope(self));

		size_t i = 0;
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = sym->type->func.args.types[i];
			for (auto name: arg.names)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				auto arg_symbol = _glsl_find_symbol(self, name.str);
				mn::print_to(self.out, "{}", _glsl_write_field(arg_type, arg_symbol->name));
				++i;
			}
		}

		mn::print_to(self.out, ")");

		if (d->func_decl.body != nullptr)
		{
			mn::print_to(self.out, " ");
			_glsl_gen_block_stmt(self, d->func_decl.body);
		}
	}

	inline static void
	_glsl_var_gen(GLSL& self, Symbol* sym)
	{
		mn::print_to(self.out, "{}", _glsl_write_field(sym->type, sym->name));
		if (sym->var_sym.value != nullptr)
		{
			mn::print_to(self.out, ";");
			_glsl_newline(self);
			_glsl_assign(self, sym, "=", sym->var_sym.value);
		}
	}

	inline static void
	_glsl_symbol_gen(GLSL& self, Symbol* sym)
	{
		switch (sym->kind)
		{
		case Symbol::KIND_FUNC:
			_glsl_func_gen(self, sym);
			break;
		case Symbol::KIND_VAR:
			_glsl_var_gen(self, sym);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}


	// API
	GLSL
	glsl_new(Unit_Package* unit, mn::Stream out)
	{
		GLSL self{};
		self.unit = unit;
		self.out = out;
		self.indent = 0;
		self.scope_stack = mn::buf_new<Scope*>();

		// push global scope as first entry in scope stack
		auto global_scope = self.unit->global_scope;
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

	inline static void
	_glsl_gen_decl_stmt(GLSL& self, Stmt* s)
	{
		auto scope = _glsl_current_scope(self);
		auto d = s->decl_stmt;
		switch (d->kind)
		{
		case Decl::KIND_VAR:
		{
			for (size_t i = 0; i < d->var_decl.names.count; ++i)
			{
				if (i > 0)
				{
					mn::print_to(self.out, ";");
					_glsl_newline(self);
				}
				auto name = d->var_decl.names[i];
				_glsl_symbol_gen(self, scope_find(scope, name.str));
			}
			break;
		}
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
			_glsl_gen_assign_stmt(self, s);
			break;
		case Stmt::KIND_EXPR:
			glsl_expr_gen(self, s->expr_stmt);
			break;
		case Stmt::KIND_BLOCK:
			_glsl_gen_block_stmt(self, s);
			break;
		case Stmt::KIND_DECL:
			_glsl_gen_decl_stmt(self, s);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	void
	glsl_gen(GLSL& self)
	{
		for (size_t i = 0; i < self.unit->reachable_symbols.count; ++i)
		{
			if (i > 0)
			{
				_glsl_newline(self);
			}

			_glsl_symbol_gen(self, self.unit->reachable_symbols[i]);
		}
	}
}