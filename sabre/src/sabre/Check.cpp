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
			return type_float32;
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
		case Expr::KIND_CALL:
		case Expr::KIND_CAST:
		case Expr::KIND_DOT:
		case Expr::KIND_INDEXED:
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
			_typer_resolve_const(self, sym);
			break;
		case Symbol::KIND_VAR:

			break;
		case Symbol::KIND_FUNC:
			break;
		default:
			assert(false && "unreachable");
			break;
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
		return;
	}
}