#include "sabre/Check.h"
#include "sabre/Unit.h"

#include <mn/IO.h>

namespace sabre
{
	inline static Scope*
	_typer_current_scope(const Typer& self)
	{
		return mn::buf_top(self.scope_stack);
	}

	inline static void
	_typer_enter_scope(Typer& self, Scope* scope)
	{
		assert(scope != nullptr);
		mn::buf_push(self.scope_stack, scope);
	}

	inline static void
	_typer_leave_scope(Typer& self)
	{
		assert(self.scope_stack.count > 1);
		mn::buf_pop(self.scope_stack);
	}

	inline static Type*
	_typer_expected_return_type(const Typer& self)
	{
		for (auto it = _typer_current_scope(self); it != nullptr; it = it->parent)
		{
			if (it->expected_type != nullptr)
				return it->expected_type;
		}
		return nullptr;
	}

	inline static Symbol*
	_typer_add_symbol(Typer& self, Symbol* sym)
	{
		auto current_scope = _typer_current_scope(self);
		if (auto old_sym = scope_shallow_find(current_scope, sym->name))
		{
			auto old_loc = symbol_location(old_sym);
			Err err{};
			err.loc = symbol_location(sym);
			err.loc = old_loc;
			if (old_loc.pos.line > 0)
				err.msg = mn::strf("'{}' symbol redefinition, first declared in {}:{}", sym->name, old_loc.pos.line, old_loc.pos.col);
			else
				err.msg = mn::strf("'{}' symbol redefinition", sym->name);
			unit_err(self.unit, err);
			return old_sym;
		}
		scope_add(current_scope, sym);
		return sym;
	}

	inline static Symbol*
	_typer_find_symbol(const Typer& self, const char* name)
	{
		return scope_find(_typer_current_scope(self), name);
	}

	inline static void
	_typer_resolve_symbol(Typer& self, Symbol* sym);

	inline static Type*
	_typer_resolve_expr(Typer& self, Expr* e);

	inline static void
	_typer_shallow_process_decl(Typer& self, Decl* decl)
	{
		switch (decl->kind)
		{
		case Decl::KIND_CONST:
			for (size_t i = 0; i < decl->const_decl.names.count; ++i)
			{
				// TODO(Moustapha): this assumes that we don't have multiple return values
				auto name = decl->const_decl.names[i];
				auto sign = decl->const_decl.type;
				Expr* value = nullptr;
				if (i < decl->const_decl.values.count)
					value = decl->const_decl.values[i];
				_typer_add_symbol(self, symbol_const_new(self.unit->symbols_arena, name, decl, sign, value));
			}
			break;
		case Decl::KIND_VAR:
			for (size_t i = 0; i < decl->var_decl.names.count; ++i)
			{
				// TODO(Moustapha): this assumes that we don't have multiple return values
				auto name = decl->var_decl.names[i];
				auto sign = decl->var_decl.type;
				Expr* value = nullptr;
				if (i < decl->var_decl.values.count)
					value = decl->var_decl.values[i];
				_typer_add_symbol(self, symbol_const_new(self.unit->symbols_arena, name, decl, sign, value));
			}
			break;
		case Decl::KIND_FUNC:
			_typer_add_symbol(self, symbol_func_new(self.unit->symbols_arena, decl->name, decl));
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static Type*
	_typer_resolve_type_sign(Typer& self, const Type_Sign& sign)
	{
		auto res = type_void;
		for (size_t i = 0; i < sign.atoms.count; ++i)
		{
			const auto& atom = sign.atoms[sign.atoms.count - i - 1];
			switch (atom.kind)
			{
			case Type_Sign_Atom::KIND_NAMED:
				// this maybe a basic type
				res = type_from_name(atom.named);
				if (type_is_equal(res, type_void))
				{
					assert(false && "user defined types are not supported");
				}
				break;
			default:
				assert(false && "unreachable");
				break;
			}
		}
		return res;
	}

	inline static Type*
	_typer_resolve_atom_expr(Typer& self, Expr* e)
	{
		switch (e->atom.kind)
		{
		case Tkn::KIND_INTEGER:
			return type_int;
		case Tkn::KIND_FLOAT:
			return type_float;
		case Tkn::KIND_KEYWORD_FALSE:
		case Tkn::KIND_KEYWORD_TRUE:
			return type_bool;
		case Tkn::KIND_ID:
			if (auto sym = _typer_find_symbol(self, e->atom.str))
			{
				_typer_resolve_symbol(self, sym);
				return sym->type;
			}
			else
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("'{}' undefined symbol", e->atom.str);
				unit_err(self.unit, err);
				return type_void;
			}
		default:
			assert(false && "unreachable");
			return type_void;
		}
	}

	inline static Type*
	_typer_resolve_binary_expr(Typer& self, Expr* e)
	{
		auto lhs_type = _typer_resolve_expr(self, e->binary.left);
		auto rhs_type = _typer_resolve_expr(self, e->binary.right);

		if (type_is_equal(lhs_type, rhs_type) == false)
		{
			Err err{};
			err.loc = e->loc;
			err.msg = mn::strf("type mismatch in binary expression, lhs is '{}' and rhs is '{}'", lhs_type, rhs_type);
			unit_err(self.unit, err);
		}

		if (e->binary.op.kind == Tkn::KIND_LOGICAL_AND ||
			e->binary.op.kind == Tkn::KIND_LOGICAL_OR)
		{
			if (type_is_equal(lhs_type, type_bool) == false)
			{
				Err err{};
				err.loc = e->binary.left->loc;
				err.msg = mn::strf("logical operators only work on boolean types, but found '{}'", lhs_type);
				unit_err(self.unit, err);
			}

			if (type_is_equal(rhs_type, type_bool) == false)
			{
				Err err{};
				err.loc = e->binary.right->loc;
				err.msg = mn::strf("logical operators only work on boolean types, but found '{}'", rhs_type);
				unit_err(self.unit, err);
			}
		}

		if (e->binary.op.kind == Tkn::KIND_LESS ||
			e->binary.op.kind == Tkn::KIND_LESS_EQUAL ||
			e->binary.op.kind == Tkn::KIND_GREATER ||
			e->binary.op.kind == Tkn::KIND_GREATER_EQUAL ||
			e->binary.op.kind == Tkn::KIND_EQUAL_EQUAL ||
			e->binary.op.kind == Tkn::KIND_NOT_EQUAL)
		{
			return type_bool;
		}

		return lhs_type;
	}

	inline static Type*
	_typer_resolve_unary_expr(Typer& self, Expr* e)
	{
		auto type = _typer_resolve_expr(self, e->unary.base);

		// works with numbers
		if (e->unary.op.kind == Tkn::KIND_INC ||
			e->unary.op.kind == Tkn::KIND_DEC ||
			e->unary.op.kind == Tkn::KIND_PLUS ||
			e->unary.op.kind == Tkn::KIND_MINUS)
		{
			if (type_is_numeric(type) == false)
			{
				Err err{};
				err.loc = e->unary.base->loc;
				err.msg = mn::strf("'{}' is only allowed for numeric types, but expression type is '{}'", e->unary.op.str, type);
				unit_err(self.unit, err);
			}
		}
		else if (e->unary.op.kind == Tkn::KIND_LOGICAL_NOT)
		{
			if (type_is_equal(type, type_bool) == false)
			{
				Err err{};
				err.loc = e->unary.base->loc;
				err.msg = mn::strf("logical not operator is only allowed for boolean types, but expression type is '{}'", e->unary.op.str, type);
				unit_err(self.unit, err);
			}
		}

		return type;
	}

	inline static Type*
	_typer_resolve_call_expr(Typer& self, Expr* e)
	{
		auto type = _typer_resolve_expr(self, e->call.base);

		if (type_is_func(type) == false)
		{
			Err err{};
			err.loc = e->call.base->loc;
			err.msg = mn::strf("invalid call, expression is not a function");
			unit_err(self.unit, err);
			return type_void;
		}

		if (e->call.args.count != type->func.args.count)
		{
			Err err{};
			err.loc = e->loc;
			err.msg = mn::strf("function expected {} arguments, but {} were provided", type->func.args.count, e->call.args.count);
			unit_err(self.unit, err);
			return type->func.return_type;
		}

		for (size_t i = 0; i < e->call.args.count; ++i)
		{
			auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
			if (type_is_equal(arg_type, type->func.args[i]) == false)
			{
				Err err{};
				err.loc = e->call.args[i]->loc;
				err.msg = mn::strf("function argument #{} type mismatch, expected '{}' but found '{}'", i, type->func.args[i], arg_type);
				unit_err(self.unit, err);
			}
		}

		return type->func.return_type;
	}

	inline static Type*
	_typer_resolve_cast_expr(Typer& self, Expr* e)
	{
		auto from_type = _typer_resolve_expr(self, e->cast.base);
		auto to_type = _typer_resolve_type_sign(self, e->cast.type);

		if (type_is_numeric(from_type) && type_is_numeric(to_type))
			return to_type;

		Err err{};
		err.loc = e->loc;
		err.msg = mn::strf("cannot cast '{}' to '{}'", from_type, to_type);
		unit_err(self.unit, err);
		return to_type;
	}

	inline static Type*
	_typer_resolve_dot_expr(Typer& self, Expr* e)
	{
		Err err{};
		err.loc = e->loc;
		err.msg = mn::strf("structures are not supported yet");
		unit_err(self.unit, err);
		return type_void;
	}

	inline static Type*
	_typer_resolve_indexed_expr(Typer& self, Expr* e)
	{
		Err err{};
		err.loc = e->loc;
		err.msg = mn::strf("arrays are not supported yet");
		unit_err(self.unit, err);
		return type_void;
	}

	inline static Type*
	_typer_resolve_expr(Typer& self, Expr* e)
	{
		if (e->type != nullptr)
			return e->type;

		switch (e->kind)
		{
		case Expr::KIND_ATOM:
			e->type = _typer_resolve_atom_expr(self, e);
			return e->type;
		case Expr::KIND_BINARY:
			e->type = _typer_resolve_binary_expr(self, e);
			return e->type;
		case Expr::KIND_UNARY:
			e->type = _typer_resolve_unary_expr(self, e);
			return e->type;
		case Expr::KIND_CALL:
			e->type = _typer_resolve_call_expr(self, e);
			return e->type;
		case Expr::KIND_CAST:
			e->type = _typer_resolve_cast_expr(self, e);
			return e->type;
		case Expr::KIND_DOT:
			e->type = _typer_resolve_dot_expr(self, e);
			return e->type;
		case Expr::KIND_INDEXED:
			e->type = _typer_resolve_indexed_expr(self, e);
			return e->type;
		default:
			assert(false && "unreachable");
			return type_void;
		}
	}

	inline static Type*
	_typer_resolve_const(Typer& self, Symbol* sym)
	{
		// we should infer if the declaration has no type signature
		auto infer = sym->const_sym.sign.atoms.count == 0;

		auto res = type_void;
		if (infer == false)
			res = _typer_resolve_type_sign(self, sym->const_sym.sign);

		auto e = sym->const_sym.value;
		if (infer)
		{
			if (e != nullptr)
			{
				sym->type = _typer_resolve_expr(self, e);
			}
			else
			{
				Err err{};
				err.loc = symbol_location(sym);
				err.msg = mn::strf("no expression to infer the type of the constant from");
				unit_err(self.unit, err);
			}
		}
		else
		{
			if (e != nullptr)
			{
				auto expr_type = _typer_resolve_expr(self, e);
				if (type_is_equal(expr_type, res) == false)
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("type mismatch expected '{}' but found '{}'", res, expr_type);
					unit_err(self.unit, err);
				}
			}
			sym->type = res;
		}

		return res;
	}

	inline static Type*
	_typer_resolve_var(Typer& self, Symbol* sym)
	{
		// we should infer if the declaration has no type signature
		auto infer = sym->var_sym.sign.atoms.count == 0;

		auto res = type_void;
		if (infer == false)
			res = _typer_resolve_type_sign(self, sym->var_sym.sign);

		auto e = sym->var_sym.value;
		if (infer)
		{
			if (e != nullptr)
			{
				sym->type = _typer_resolve_expr(self, e);
			}
			else
			{
				Err err{};
				err.loc = symbol_location(sym);
				err.msg = mn::strf("no expression to infer the type of the constant from");
				unit_err(self.unit, err);
			}
		}
		else
		{
			if (e != nullptr)
			{
				auto expr_type = _typer_resolve_expr(self, e);
				if (type_is_equal(expr_type, res) == false)
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("type mismatch expected '{}' but found '{}'", res, expr_type);
					unit_err(self.unit, err);
				}
			}
			sym->type = res;
		}

		return res;
	}

	inline static Type*
	_typer_resolve_func_decl(Typer& self, Symbol* sym)
	{
		auto d = sym->func_sym.decl;

		auto sign = func_sign_new();
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = _typer_resolve_type_sign(self, arg.type);
			for (size_t i = 0; i < arg.names.count; ++i)
				mn::buf_push(sign.args, arg_type);
		}
		sign.return_type = _typer_resolve_type_sign(self, d->func_decl.return_type);
		return type_interner_func(self.unit->type_interner, sign);
	}

	inline static Type*
	_typer_resolve_stmt(Typer& self, Stmt* s);

	inline static Type*
	_typer_resolve_break_stmt(Typer& self, Stmt* s)
	{
		auto scope = _typer_current_scope(self);
		if (scope->flags != Scope::FLAG_INSIDE_LOOP)
		{
			Err err{};
			err.loc = s->loc;
			err.msg = mn::strf("unexpected break statement, they can only appear in for loops");
			unit_err(self.unit, err);
		}
		return type_void;
	}

	inline static Type*
	_typer_resolve_continue_stmt(Typer& self, Stmt* s)
	{
		auto scope = _typer_current_scope(self);
		if (scope->flags != Scope::FLAG_INSIDE_LOOP)
		{
			Err err{};
			err.loc = s->loc;
			err.msg = mn::strf("unexpected continue statement, they can only appear in for loops");
			unit_err(self.unit, err);
		}
		return type_void;
	}

	inline static Type*
	_typer_resolve_return_stmt(Typer& self, Stmt* s)
	{
		auto ret = _typer_resolve_expr(self, s->return_stmt);
		auto expected = _typer_expected_return_type(self);
		if (expected == nullptr)
		{
			Err err{};
			err.loc = s->loc;
			err.msg = mn::strf("unexpected return statement");
			unit_err(self.unit, err);
			return ret;
		}

		if (type_is_equal(ret, expected) == false)
		{
			Err err{};
			err.loc = s->return_stmt->loc;
			err.msg = mn::strf("incorrect return type '{}' expected '{}'", ret, expected);
			unit_err(self.unit, err);
		}

		return ret;
	}

	inline static Type*
	_typer_resolve_if_stmt(Typer& self, Stmt* s)
	{
		if (s->if_stmt.cond.count != s->if_stmt.body.count)
		{
			Err err{};
			err.loc = s->loc;
			err.msg = mn::strf("missing if condition");
			unit_err(self.unit, err);
			return type_void;
		}

		for (size_t i = 0; i < s->if_stmt.cond.count; ++i)
		{
			auto cond_type = _typer_resolve_expr(self, s->if_stmt.cond[i]);
			if (type_is_equal(cond_type, type_bool) == false)
			{
				Err err{};
				err.loc = s->if_stmt.cond[i]->loc;
				err.msg = mn::strf("if condition type '{}' is not a boolean", cond_type);
				unit_err(self.unit, err);
			}
			_typer_resolve_stmt(self, s->if_stmt.body[i]);
		}

		if (s->if_stmt.else_body != nullptr)
			_typer_resolve_stmt(self, s->if_stmt.else_body);
		return type_void;
	}

	inline static Type*
	_typer_resolve_for_stmt(Typer& self, Stmt* s)
	{
		auto scope = unit_create_scope_for(self.unit, s, _typer_current_scope(self), "for loop", nullptr, Scope::FLAG_INSIDE_LOOP);
		_typer_enter_scope(self, scope);
		{
			if (s->for_stmt.init != nullptr)
				_typer_resolve_stmt(self, s->for_stmt.init);

			if (s->for_stmt.cond != nullptr)
			{
				auto cond_type = _typer_resolve_expr(self, s->for_stmt.cond);
				if (type_is_equal(cond_type, type_bool) == false)
				{
					Err err{};
					err.loc = s->for_stmt.cond->loc;
					err.msg = mn::strf("for loop condition type '{}' is not a boolean", cond_type);
					unit_err(self.unit, err);
				}
			}

			if (s->for_stmt.post != nullptr)
				_typer_resolve_stmt(self, s->for_stmt.post);

			for (auto stmt: s->for_stmt.body->block_stmt)
				_typer_resolve_stmt(self, stmt);
		}
		_typer_leave_scope(self);
		return type_void;
	}

	inline static Type*
	_typer_resolve_assign_stmt(Typer& self, Stmt* s)
	{
		for (size_t i = 0; i < s->assign_stmt.lhs.count; ++i)
		{
			auto lhs_type = _typer_resolve_expr(self, s->assign_stmt.lhs[i]);
			if (type_is_equal(lhs_type, type_void))
			{
				Err err{};
				err.loc = s->assign_stmt.lhs[i]->loc;
				err.msg = mn::strf("cannot assign into a void type");
				unit_err(self.unit, err);
			}

			auto rhs_type = _typer_resolve_expr(self, s->assign_stmt.rhs[i]);
			if (type_is_equal(rhs_type, type_void))
			{
				Err err{};
				err.loc = s->assign_stmt.rhs[i]->loc;
				err.msg = mn::strf("cannot assign a void type");
				unit_err(self.unit, err);
			}

			if (type_is_equal(lhs_type, rhs_type))
			{
				Err err{};
				err.loc = s->assign_stmt.rhs[i]->loc;
				err.msg = mn::strf("type mismatch in assignment statement, expected '{}' but found '{}'", lhs_type, rhs_type);
				unit_err(self.unit, err);
			}
		}
		return type_void;
	}

	inline static Type*
	_typer_resolve_decl_stmt(Typer& self, Stmt* s)
	{
		auto d = s->decl_stmt;
		switch (d->kind)
		{
		case Decl::KIND_CONST:
			for (size_t i = 0; i < d->const_decl.names.count; ++i)
			{
				auto name = d->const_decl.names[i];
				Expr* value = nullptr;
				if (i < d->const_decl.values.count)
					value = d->const_decl.values[i];
				auto sym = symbol_const_new(self.unit->symbols_arena, name, d, d->const_decl.type, value);
				_typer_resolve_symbol(self, sym);
				_typer_add_symbol(self, sym);
			}
			break;
		case Decl::KIND_VAR:
			for (size_t i = 0; i < d->var_decl.names.count; ++i)
			{
				auto name = d->var_decl.names[i];
				Expr* value = nullptr;
				if (i < d->var_decl.values.count)
					value = d->var_decl.values[i];
				auto sym = symbol_const_new(self.unit->symbols_arena, name, d, d->var_decl.type, value);
				_typer_resolve_symbol(self, sym);
				_typer_add_symbol(self, sym);
			}
			break;
		case Decl::KIND_FUNC:
		{
			auto sym = symbol_func_new(self.unit->symbols_arena, d->name, d);
			_typer_resolve_symbol(self, sym);
			_typer_add_symbol(self, sym);
			break;
		}
		default:
			assert(false && "unreachable");
			break;
		}
		return type_void;
	}

	inline static Type*
	_typer_resolve_stmt(Typer& self, Stmt* s)
	{
		switch (s->kind)
		{
		case Stmt::KIND_BREAK:
			return _typer_resolve_break_stmt(self, s);
		case Stmt::KIND_CONTINUE:
			return _typer_resolve_continue_stmt(self, s);
		case Stmt::KIND_RETURN:
			return _typer_resolve_return_stmt(self, s);
		case Stmt::KIND_IF:
			return _typer_resolve_if_stmt(self, s);
		case Stmt::KIND_FOR:
			return _typer_resolve_for_stmt(self, s);
		case Stmt::KIND_ASSIGN:
			return _typer_resolve_assign_stmt(self, s);
		case Stmt::KIND_EXPR:
			return _typer_resolve_expr(self, s->expr_stmt);
		case Stmt::KIND_DECL:
			return _typer_resolve_decl_stmt(self, s);
		default:
			assert(false && "unreachable");
			return type_void;
		}
	}

	inline static void
	_typer_resolve_func_body(Typer& self, Symbol* sym)
	{
		auto d = sym->func_sym.decl;

		auto scope = unit_create_scope_for(self.unit, d, _typer_current_scope(self), d->name.str, sym->type->func.return_type, Scope::FLAG_NONE);
		_typer_enter_scope(self, scope);
		{
			// push function arguments into scope
			size_t i = 0;
			for (auto arg: d->func_decl.args)
			{
				auto arg_type = sym->type->func.args[i];
				for (auto name: arg.names)
				{
					auto v = symbol_var_new(self.unit->symbols_arena, name, nullptr, arg.type, nullptr);
					v->type = arg_type;
					v->state = Symbol::STATE_RESOLVED;
					_typer_add_symbol(self, v);
					++i;
				}
			}

			// typecheck function body if it exists
			if (d->func_decl.body != nullptr)
			{
				for (auto stmt: d->func_decl.body->block_stmt)
					_typer_resolve_stmt(self, stmt);

				if (type_is_equal(sym->type->func.return_type, type_void) == false)
				{
					// check stmt will terminate?
				}
			}
		}
		_typer_leave_scope(self);
	}

	inline static void
	_typer_resolve_symbol(Typer& self, Symbol* sym)
	{
		if (sym->state == Symbol::STATE_RESOLVED)
		{
			return;
		}
		else if (sym->state == Symbol::STATE_RESOLVING)
		{
			Err err{};
			err.loc = symbol_location(sym);
			err.msg = mn::strf("'{}' cyclic dependency", sym->name);
			unit_err(self.unit, err);
			return;
		}

		sym->state = Symbol::STATE_RESOLVING;
		switch (sym->kind)
		{
		case Symbol::KIND_CONST:
			sym->type = _typer_resolve_const(self, sym);
			break;
		case Symbol::KIND_VAR:
			sym->type = _typer_resolve_var(self, sym);
			break;
		case Symbol::KIND_FUNC:
			sym->type = _typer_resolve_func_decl(self, sym);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
		sym->state = Symbol::STATE_RESOLVED;

		switch(sym->kind)
		{
		case Symbol::KIND_FUNC:
			_typer_resolve_func_body(self, sym);
			break;
		case Symbol::KIND_VAR:
		case Symbol::KIND_CONST:
			// do nothing
			break;
		default:
			assert(false && "unreachable");
			break;
		}

		// if sym is top level we add it to reachable symbols
		if (scope_is_top_level(self.global_scope, sym))
		{
			mn::buf_push(self.unit->reachable_symbols, sym);
		}
	}

	inline static void
	_typer_shallow_walk(Typer& self)
	{
		for (auto decl: self.unit->decls)
			_typer_shallow_process_decl(self, decl);

		for (auto sym: self.global_scope->symbols)
		{
			_typer_resolve_symbol(self, sym);
		}
	}

	// API
	Typer
	typer_new(Unit* unit)
	{
		Typer self{};
		self.unit = unit;
		self.global_scope = unit->global_scope;

		mn::buf_push(self.scope_stack, self.global_scope);
		return self;
	}

	void
	typer_free(Typer& self)
	{
		mn::buf_free(self.scope_stack);
	}

	void
	typer_check(Typer& self)
	{
		_typer_shallow_walk(self);

		// library mode for now
		for (auto sym: self.global_scope->symbols)
			_typer_resolve_symbol(self, sym);

		return;
	}
}