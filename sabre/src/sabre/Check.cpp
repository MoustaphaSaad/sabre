#include "sabre/Check.h"
#include "sabre/Unit.h"

#include <mn/IO.h>

namespace sabre
{
	const char SWIZZLE_XYZW[4] = {'x', 'y', 'z', 'w'};
	const char SWIZZLE_RGBA[4] = {'r', 'g', 'b', 'a'};

	inline static bool
	_swizzle_style_contains(const char* style, size_t size, mn::Rune r)
	{
		for (size_t i = 0; i < size; ++i)
			if (style[i] == r)
				return true;
		return false;
	}

	inline static const char*
	_choose_swizzle_style(mn::Rune r)
	{
		if (_swizzle_style_contains(SWIZZLE_XYZW, 4, r))
			return SWIZZLE_XYZW;

		if (_swizzle_style_contains(SWIZZLE_RGBA, 4, r))
			return SWIZZLE_RGBA;

		return nullptr;
	}

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
	_typer_resolve_func_decl(Typer& self, Decl* d);

	inline static Type*
	_typer_resolve_expr(Typer& self, Expr* e);

	inline static void
	_typer_resolve_func_body_internal(Typer& self, Decl* d, Type* t);

	inline static void
	_typer_add_func_overload(Typer& self, Type* overload_set, Decl* decl)
	{
		Type_Overload_Entry overload_entry{};
		overload_entry.loc = decl->loc;
		overload_entry.type = _typer_resolve_func_decl(self, decl);

		if (auto it = mn::map_lookup(overload_set->func_overload_set_type.overloads, overload_entry.type->func.args))
		{
			auto old_loc = it->value.loc;
			Err err{};
			err.loc = decl->loc;
			err.msg = mn::strf("function overload already defined {}:{}:{}", old_loc.file->filepath, old_loc.pos.line, old_loc.pos.col);
			unit_err(self.unit, err);
		}
		else
		{
			mn::map_insert(overload_set->func_overload_set_type.overloads, overload_entry.type->func.args, overload_entry);
		}
	}

	inline static Symbol*
	_typer_add_func_symbol(Typer& self, Decl* decl)
	{
		assert(decl->kind == Decl::KIND_FUNC);

		// try to find a symbol with the same name
		auto sym = _typer_find_symbol(self, decl->name.str);
		// if we didn't find any function with this name then we'll try to add a symbol
		if (sym == nullptr || (sym->kind != Symbol::KIND_FUNC && sym->kind != Symbol::KIND_FUNC_OVERLOAD_SET))
			return _typer_add_symbol(self, symbol_func_new(self.unit->symbols_arena, decl->name, decl));

		if (sym->kind == Symbol::KIND_FUNC)
		{
			// convert the function symbol to overload set
			if (sym->func_sym.decl != decl)
				sym = symbol_func_overload_set_new(self.unit->symbols_arena, sym);
			else
				return sym;
		}

		assert(sym->kind == Symbol::KIND_FUNC_OVERLOAD_SET);
		// add the function declaration to overload set
		auto decl_type = _typer_resolve_func_decl(self, decl);
		mn::map_insert(sym->func_overload_set_sym.decls, decl, decl_type);
		if (sym->state == Symbol::STATE_RESOLVED)
		{
			assert(sym->type->kind == Type::KIND_FUNC_OVERLOAD_SET);
			_typer_add_func_overload(self, sym->type, decl);
			_typer_resolve_func_body_internal(self, decl, decl_type);
		}
		return sym;
	}

	inline static void
	_typer_shallow_process_decl(Typer& self, Unit_File* file, Decl* decl)
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
			_typer_add_func_symbol(self, decl);
			break;
		case Decl::KIND_STRUCT:
			_typer_add_symbol(self, symbol_struct_new(self.unit->symbols_arena, decl->name, decl));
			break;
		case Decl::KIND_IMPORT:
		{
			// TODO(Moustapha): unescape the string
			auto package_path = mn::str_from_c(decl->import_decl.path.str, mn::memory::tmp());
			mn::str_trim(package_path, "\"");

			auto [package, resolve_err] = unit_file_resolve_package(file, package_path, decl->import_decl.name);
			if (resolve_err == false)
				_typer_add_symbol(self, symbol_package_new(self.unit->symbols_arena, decl->name, decl, package));
			break;
		}
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
				// type from imported package
				if (atom.named.package_name)
				{
					// package sym
					auto package_sym = _typer_find_symbol(self, atom.named.package_name.str);
					if (package_sym == nullptr)
					{
						Err err{};
						err.loc = atom.named.package_name.loc;
						err.msg = mn::strf("'{}' undefined symbol", atom.named.package_name.str);
						unit_err(self.unit, err);
						break;
					}

					if (package_sym->kind != Symbol::KIND_PACKAGE)
					{
						Err err{};
						err.loc = atom.named.package_name.loc;
						err.msg = mn::strf("'{}' is not an imported package", atom.named.package_name.str);
						unit_err(self.unit, err);
						break;
					}

					auto package = package_sym->package_sym.package;
					auto type_symbol = scope_shallow_find(package->global_scope, atom.named.type_name.str);
					if (type_symbol == nullptr)
					{
						Err err{};
						err.loc = atom.named.type_name.loc;
						err.msg = mn::strf("'{}' undefined symbol", atom.named.type_name.str);
						unit_err(self.unit, err);
						break;
					}

					_typer_resolve_symbol(self, type_symbol);
					res = type_symbol->type;
				}
				else
				{
					// this maybe a basic type
					res = type_from_name(atom.named.type_name);
					if (type_is_equal(res, type_void))
					{
						if (auto symbol = _typer_find_symbol(self, atom.named.type_name.str))
						{
							_typer_resolve_symbol(self, symbol);
							res = symbol->type;
						}
						else
						{
							Err err{};
							err.loc = atom.named.type_name.loc;
							err.msg = mn::strf("'{}' undefined symbol", atom.named.type_name.str);
							unit_err(self.unit, err);
						}
					}
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
		case Tkn::KIND_LITERAL_INTEGER:
			e->mode = ADDRESS_MODE_CONST;
			e->const_value = expr_value_int(::strtoll(e->atom.str, nullptr, 10));
			return type_int;
		case Tkn::KIND_LITERAL_FLOAT:
			e->mode = ADDRESS_MODE_CONST;
			e->const_value = expr_value_int(::strtod(e->atom.str, nullptr));
			return type_float;
		case Tkn::KIND_KEYWORD_FALSE:
			e->mode = ADDRESS_MODE_CONST;
			e->const_value = expr_value_bool(false);
			return type_bool;
		case Tkn::KIND_KEYWORD_TRUE:
			e->mode = ADDRESS_MODE_CONST;
			e->const_value = expr_value_bool(true);
			return type_bool;
		case Tkn::KIND_ID:
			if (auto sym = _typer_find_symbol(self, e->atom.str))
			{
				_typer_resolve_symbol(self, sym);
				if (sym->kind == Symbol::KIND_CONST && sym->const_sym.value != nullptr)
				{
					e->const_value = sym->const_sym.value->const_value;
				}

				if (sym->kind == Symbol::KIND_CONST)
					e->mode = ADDRESS_MODE_CONST;
				else if (sym->kind == Symbol::KIND_VAR)
					e->mode = ADDRESS_MODE_VARIABLE;
				else if (sym->kind == Symbol::KIND_FUNC && sym->type->func.return_type != type_void)
					e->mode = ADDRESS_MODE_COMPUTED_VALUE;
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

		// handle matrix vector multiplication
		if (e->binary.op.kind == Tkn::KIND_STAR)
		{
			if (lhs_type->kind == Type::KIND_MAT && rhs_type->kind == Type::KIND_VEC)
			{
				if (lhs_type->mat.width == rhs_type->vec.width)
				{
					return rhs_type;
				}
				else
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("width mismatch in multiply operation '{}' * '{}'", lhs_type, rhs_type);
					unit_err(self.unit, err);
				}
			}
			else if (lhs_type->kind == Type::KIND_VEC && rhs_type->kind == Type::KIND_MAT)
			{
				if (lhs_type->vec.width == rhs_type->mat.width)
				{
					return lhs_type;
				}
				else
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("width mismatch in multiply operation '{}' * '{}'", lhs_type, rhs_type);
					unit_err(self.unit, err);
				}
			}
		}

		if (type_is_equal(lhs_type, rhs_type) == false)
		{
			// TODO(Moustapha): better error message here, highlight parts of the expression with their types
			Err err{};
			err.loc = e->loc;
			err.msg = mn::strf("type mismatch in binary expression, lhs is '{}' and rhs is '{}'", lhs_type, rhs_type);
			unit_err(self.unit, err);
		}

		if (e->binary.op.kind == Tkn::KIND_LOGICAL_AND ||
			e->binary.op.kind == Tkn::KIND_LOGICAL_OR)
		{
			if (type_is_bool_like(lhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.left->loc;
				err.msg = mn::strf("logical operators only work on boolean types, but found '{}'", lhs_type);
				unit_err(self.unit, err);
			}

			if (type_is_bool_like(rhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.right->loc;
				err.msg = mn::strf("logical operators only work on boolean types, but found '{}'", rhs_type);
				unit_err(self.unit, err);
			}
		}

		if (type_is_bool_like(lhs_type) || type_is_bool_like(rhs_type))
		{
			if (e->binary.op.kind != Tkn::KIND_LOGICAL_AND &&
				e->binary.op.kind != Tkn::KIND_LOGICAL_OR)
			{
				Err err{};
				err.loc = e->binary.op.loc;
				err.msg = mn::strf("boolean types doesn't support such operator");
				unit_err(self.unit, err);
			}
		}

		if (e->binary.left->mode == ADDRESS_MODE_CONST && e->binary.right->mode == ADDRESS_MODE_CONST)
		{
			e->const_value = expr_value_binar_op(e->binary.left->const_value, e->binary.op.kind, e->binary.right->const_value);
			e->mode = ADDRESS_MODE_CONST;
		}
		else
		{
			e->mode = ADDRESS_MODE_COMPUTED_VALUE;
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
		if (e->unary.op.kind == Tkn::KIND_PLUS ||
			e->unary.op.kind == Tkn::KIND_MINUS)
		{
			if (type_can_negate(type) == false)
			{
				Err err{};
				err.loc = e->unary.base->loc;
				err.msg = mn::strf("'{}' is only allowed for numeric types, but expression type is '{}'", e->unary.op.str, type);
				unit_err(self.unit, err);
			}
		}
		if (e->unary.op.kind == Tkn::KIND_INC ||
			e->unary.op.kind == Tkn::KIND_DEC)
		{
			if (type_can_increment(type) == false)
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

		if (e->unary.base->mode == ADDRESS_MODE_CONST && (e->unary.op.kind == Tkn::KIND_INC || e->unary.op.kind == Tkn::KIND_DEC))
		{
			Err err{};
			err.loc = e->loc;
			err.msg = mn::strf("cannot evaluate expression in compile time");
			unit_err(self.unit, err);
		}

		if (e->unary.base->const_value.kind != Expr_Value::KIND_NONE)
			e->const_value = expr_value_unary_op(e->unary.base->const_value, e->unary.op.kind);

		if (e->unary.base->mode == ADDRESS_MODE_CONST)
			e->mode = ADDRESS_MODE_CONST;
		else
			e->mode = ADDRESS_MODE_COMPUTED_VALUE;

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

		e->mode = ADDRESS_MODE_COMPUTED_VALUE;

		if (type->kind == Type::KIND_FUNC)
		{
			if (e->call.args.count != type->func.args.types.count)
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("function expected {} arguments, but {} were provided", type->func.args.types.count, e->call.args.count);
				unit_err(self.unit, err);
				return type->func.return_type;
			}

			for (size_t i = 0; i < e->call.args.count; ++i)
			{
				auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
				if (type_is_equal(arg_type, type->func.args.types[i]) == false)
				{
					Err err{};
					err.loc = e->call.args[i]->loc;
					err.msg = mn::strf("function argument #{} type mismatch, expected '{}' but found '{}'", i, type->func.args.types[i], arg_type);
					unit_err(self.unit, err);
				}
			}

			return type->func.return_type;
		}
		else if (type->kind == Type::KIND_FUNC_OVERLOAD_SET)
		{
			auto overload_set_symbol = type->func_overload_set_type.symbol;
			Type* res = nullptr;
			for (auto& [overload_decl, overload_type]: overload_set_symbol->func_overload_set_sym.decls)
			{
				if (e->call.args.count != overload_type->func.args.types.count)
					continue;

				bool args_match = true;
				for (size_t i = 0; i < e->call.args.count; ++i)
				{
					auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
					if (type_is_equal(arg_type, overload_type->func.args.types[i]) == false)
					{
						args_match = false;
						break;
					}
				}
				if (args_match)
				{
					res = overload_type->func.return_type;
					mn::buf_push(overload_set_symbol->func_overload_set_sym.used_decls, overload_decl);
					break;
				}
			}

			if (res == nullptr)
			{
				auto msg = mn::strf("cannot find suitable function for 'func(");

				for (size_t i = 0; i < e->call.args.count; ++i)
				{
					if (i > 0)
						msg = mn::strf(msg, ", ");

					auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
					msg = mn::strf(msg, ":{}", arg_type);
				}
				msg = mn::strf(msg, ")' in the overload set:");

				auto overload_i = 0;
				for (auto [_, overload]: type->func_overload_set_type.overloads)
				{
					msg = mn::strf(
						msg,
						"\n  {}. {} defined in {}:{}:{}",
						overload_i++,
						overload.type,
						overload.loc.file->filepath,
						overload.loc.pos.line,
						overload.loc.pos.col
					);
				}

				Err err{};
				err.loc = e->loc;
				err.msg = msg;
				unit_err(self.unit, err);
				return type_void;
			}
			else
			{
				return res;
			}
		}
		return type_void;
	}

	inline static Type*
	_typer_resolve_cast_expr(Typer& self, Expr* e)
	{
		auto from_type = _typer_resolve_expr(self, e->cast.base);
		auto to_type = _typer_resolve_type_sign(self, e->cast.type);

		if (e->cast.base->const_value.kind != Expr_Value::KIND_NONE)
			e->const_value = e->cast.base->const_value;

		if (type_is_numeric_scalar(from_type) && type_is_numeric_scalar(to_type))
			return to_type;

		if (e->cast.base->mode == ADDRESS_MODE_CONST)
		{
			e->const_value = e->cast.base->const_value;
			e->mode = ADDRESS_MODE_CONST;
		}
		else
		{
			e->mode = e->unary.base->mode;
		}

		Err err{};
		err.loc = e->loc;
		err.msg = mn::strf("cannot cast '{}' to '{}'", from_type, to_type);
		unit_err(self.unit, err);
		return to_type;
	}

	inline static Type*
	_typer_resolve_dot_expr(Typer& self, Expr* e)
	{
		auto type = _typer_resolve_expr(self, e->dot.lhs);
		if (type->kind == Type::KIND_VEC)
		{
			bool outside_range = false;
			bool illegal = false;

			if (e->dot.rhs->kind != Expr::KIND_ATOM)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("unknown structure field");
				unit_err(self.unit, err);
				return type_void;
			}

			auto it = e->dot.rhs->atom.str;
			size_t len = 0;
			auto r = mn::rune_read(it);
			const char* swizzle_style = _choose_swizzle_style(r);
			if (swizzle_style == nullptr)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("illegal swizzle pattern");
				unit_err(self.unit, err);
				return type_void;
			}

			while (auto r = mn::rune_read(it))
			{
				it = mn::rune_next(it);
				++len;

				outside_range |= _swizzle_style_contains(swizzle_style, type->vec.width, r) == false;
				illegal |= _swizzle_style_contains(swizzle_style, 4, r) == false;
			}

			if (illegal)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("illegal vector field");
				unit_err(self.unit, err);
				return type_void;
			}
			else if (outside_range || len > 4)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("vector field out of range");
				unit_err(self.unit, err);
				return type_void;
			}

			e->mode = e->dot.lhs->mode;
			return type_vectorize(type->vec.base, len);
		}
		else if (type->kind == Type::KIND_STRUCT)
		{
			if (e->dot.rhs->kind != Expr::KIND_ATOM)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("unknown structure field");
				unit_err(self.unit, err);
				return type_void;
			}

			auto it = mn::map_lookup(type->struct_type.fields_by_name, e->dot.rhs->atom.str);
			if (it == nullptr)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("unknown structure field");
				unit_err(self.unit, err);
				return type_void;
			}

			e->mode = e->dot.lhs->mode;
			return type->struct_type.fields[it->value].type;
		}
		else if (type->kind == Type::KIND_PACKAGE)
		{
			if (e->dot.rhs->kind != Expr::KIND_ATOM)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("unknown structure field");
				unit_err(self.unit, err);
				return type_void;
			}

			auto package = type->package_type.package;
			auto symbol = scope_shallow_find(package->global_scope, e->dot.rhs->atom.str);
			if (symbol == nullptr)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("undefined symbol");
				unit_err(self.unit, err);
				return type_void;
			}
			_typer_resolve_symbol(self, symbol);
			return symbol->type;
		}
		else
		{
			Err err{};
			err.loc = e->dot.rhs->loc;
			err.msg = mn::strf("unknown structure field");
			unit_err(self.unit, err);
			return type_void;
		}
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
				res = _typer_resolve_expr(self, e);
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
		}

		if (e->const_value.kind == Expr_Value::KIND_NONE)
		{
			Err err{};
			err.loc = e->loc;
			err.msg = mn::strf("expression cannot be evaluated in compile time");
			unit_err(self.unit, err);
		}

		sym->type = res;
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
				res = _typer_resolve_expr(self, e);
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
		}

		sym->type = res;
		return res;
	}

	// TODO(Moustapha): you should cache the type of the declaration instead of calculating it everytime
	inline static Type*
	_typer_resolve_func_decl(Typer& self, Decl* d)
	{
		auto sign = func_sign_new();
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = _typer_resolve_type_sign(self, arg.type);
			for (size_t i = 0; i < arg.names.count; ++i)
				mn::buf_push(sign.args.types, arg_type);
		}
		sign.return_type = _typer_resolve_type_sign(self, d->func_decl.return_type);
		return type_interner_func(self.unit->parent_unit->type_interner, sign);
	}

	inline static Type*
	_typer_resolve_func_decl(Typer& self, Symbol* sym)
	{
		return _typer_resolve_func_decl(self, sym->func_sym.decl);
	}

	inline static Type*
	_typer_resolve_func_overload_set(Typer& self, Symbol* sym)
	{
		assert(sym->kind == Symbol::KIND_FUNC_OVERLOAD_SET);

		auto type = type_interner_overload_set(self.unit->parent_unit->type_interner, sym);
		for (auto& [decl, decl_type]: sym->func_overload_set_sym.decls)
		{
			decl_type = _typer_resolve_func_decl(self, decl);
			_typer_add_func_overload(self, type, decl);
		}
		return type;
	}

	inline static Type*
	_typer_resolve_stmt(Typer& self, Stmt* s);

	inline static Type*
	_typer_resolve_break_stmt(Typer& self, Stmt* s)
	{
		auto scope = _typer_current_scope(self);
		if (scope_find_flag(scope, Scope::FLAG_INSIDE_LOOP) == false)
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
		if (scope_find_flag(scope, Scope::FLAG_INSIDE_LOOP) == false)
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

			if (s->assign_stmt.op.kind == Tkn::KIND_STAR_EQUAL && lhs_type->kind == Type::KIND_VEC && rhs_type->kind == Type::KIND_MAT)
			{
				if (lhs_type->vec.width == rhs_type->mat.width)
				{
					// this is allowed
					continue;
				}
				else
				{
					Err err{};
					err.loc = s->loc;
					err.msg = mn::strf("width mismatch in multiply operation '{}' * '{}'", lhs_type, rhs_type);
					unit_err(self.unit, err);
				}
			}

			if (type_is_equal(lhs_type, rhs_type) == false)
			{
				Err err{};
				err.loc = s->assign_stmt.rhs[i]->loc;
				err.msg = mn::strf("type mismatch in assignment statement, expected '{}' but found '{}'", lhs_type, rhs_type);
				unit_err(self.unit, err);
			}

			switch (s->assign_stmt.lhs[i]->mode)
			{
			case ADDRESS_MODE_VARIABLE:
				// this is okay
				break;
			case ADDRESS_MODE_CONST:
			{
				Err err{};
				err.loc = s->assign_stmt.lhs[i]->loc;
				err.msg = mn::strf("cannot assign into a constant value");
				unit_err(self.unit, err);
				break;
			}
			case ADDRESS_MODE_COMPUTED_VALUE:
			{
				Err err{};
				err.loc = s->assign_stmt.lhs[i]->loc;
				err.msg = mn::strf("cannot assign into a computed value");
				unit_err(self.unit, err);
				break;
			}
			case ADDRESS_MODE_NONE:
			default:
				assert(false && "unreachable");
				break;
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
				auto sym = symbol_var_new(self.unit->symbols_arena, name, d, d->var_decl.type, value);
				_typer_resolve_symbol(self, sym);
				_typer_add_symbol(self, sym);
			}
			break;
		case Decl::KIND_FUNC:
		{
			auto sym = _typer_add_func_symbol(self, d);
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
	_typer_resolve_block_stmt_with_scope(Typer& self, Stmt* s)
	{
		auto scope = unit_create_scope_for(self.unit, s, _typer_current_scope(self), "block", nullptr, Scope::FLAG_NONE);
		_typer_enter_scope(self, scope);
		{
			for (auto stmt: s->block_stmt)
				_typer_resolve_stmt(self, stmt);
		}
		_typer_leave_scope(self);
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
		case Stmt::KIND_BLOCK:
			return _typer_resolve_block_stmt_with_scope(self, s);
		default:
			assert(false && "unreachable");
			return type_void;
		}
	}

	struct Termination_Info
	{
		bool will_return;
		Location loc;
		mn::Str msg;
	};

	inline static Termination_Info
	_typer_stmt_will_terminate(Typer& self, Stmt* s)
	{
		switch (s->kind)
		{
		case Stmt::KIND_BLOCK:
			if (s->block_stmt.count == 0)
			{
				Termination_Info info{};
				info.will_return = false;
				info.loc = s->loc;
				info.msg = mn::str_tmpf("empty block does not return");
				return info;
			}
			return _typer_stmt_will_terminate(self, mn::buf_top(s->block_stmt));
		case Stmt::KIND_RETURN:
		{
			Termination_Info info{};
			info.will_return = true;
			info.loc = s->loc;
			return info;
		}
		case Stmt::KIND_FOR:
		{
			if (s->for_stmt.cond != nullptr)
			{
				Termination_Info info{};
				info.will_return = false;
				info.loc = s->loc;
				info.msg = mn::str_tmpf("for loop with condition may not enter and thus will not return");
				return info;
			}
			auto info = _typer_stmt_will_terminate(self, s->for_stmt.body);
			if (info.loc.pos.line == 0)
				info.loc = s->loc;
			return info;
		}
		case Stmt::KIND_IF:
		{
			for (auto body: s->if_stmt.body)
			{
				auto body_info = _typer_stmt_will_terminate(self, body);

				if (body_info.will_return == false)
				{
					Termination_Info info{};
					info.will_return = false;
					info.loc = body_info.loc;
					info.msg = mn::str_tmpf("one of the if branches does not end with return statement");
					if (info.loc.pos.line == 0)
						info.loc = s->loc;
					return info;
				}
			}
			if (s->if_stmt.else_body != nullptr)
			{
				auto body_info = _typer_stmt_will_terminate(self, s->if_stmt.else_body);

				if (body_info.will_return == false)
				{
					Termination_Info info{};
					info.will_return = false;
					info.loc = body_info.loc;
					info.msg = mn::str_tmpf("one of the if branches does not end with return statement");
					if (info.loc.pos.line == 0)
						info.loc = s->loc;
					return info;
				}
			}
			else
			{
				Termination_Info info{};
				info.will_return = false;
				info.loc = s->loc;
				info.msg = mn::str_tmpf("if statement is missing else branch");
				return info;
			}

			Termination_Info info{};
			info.will_return = true;
			info.loc = s->loc;
			return info;
		}
		default:
		{
			Termination_Info info{};
			info.will_return = false;
			info.loc = s->loc;
			return info;
		}
		}
	}

	inline static void
	_typer_resolve_func_body_internal(Typer& self, Decl* d, Type* t)
	{
		auto scope = unit_create_scope_for(self.unit, d, _typer_current_scope(self), d->name.str, t->func.return_type, Scope::FLAG_NONE);
		_typer_enter_scope(self, scope);
		{
			// push function arguments into scope
			size_t i = 0;
			for (auto arg: d->func_decl.args)
			{
				auto arg_type = t->func.args.types[i];
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

				if (type_is_equal(t->func.return_type, type_void) == false)
				{
					auto return_info = _typer_stmt_will_terminate(self, d->func_decl.body);
					if (return_info.will_return == false)
					{
						Err err{};
						err.loc = return_info.loc;
						err.msg = mn::strf("missing return at the end of the function because {}", return_info.msg);
						unit_err(self.unit, err);
					}
				}
			}
		}
		_typer_leave_scope(self);
	}

	inline static void
	_typer_resolve_func_body(Typer& self, Symbol* sym)
	{
		_typer_resolve_func_body_internal(self, sym->func_sym.decl, sym->type);
	}

	inline static void
	_typer_resolve_func_overload_set_body(Typer& self, Symbol* sym)
	{
		for (auto [decl, decl_type]: sym->func_overload_set_sym.decls)
		{
			_typer_resolve_func_body_internal(self, decl, decl_type);
		}
	}

	inline static void
	_typer_complete_type(Typer& self, Symbol* sym, Location used_from)
	{
		auto type = sym->type;
		if (type->kind == Type::KIND_COMPLETING)
		{
			Err err{};
			err.loc = used_from;
			err.msg = mn::strf("'{}' is a recursive type", sym->name);
			unit_err(self.unit, err);
			return;
		}
		else if (type->kind != Type::KIND_INCOMPLETE)
		{
			return;
		}

		type->kind = Type::KIND_COMPLETING;
		if (sym->kind == Symbol::KIND_STRUCT)
		{
			auto d = sym->struct_sym.decl;
			auto struct_fields = mn::buf_with_allocator<Field_Type>(self.unit->parent_unit->type_interner.arena);
			auto struct_fields_by_name = mn::map_with_allocator<const char*, size_t>(self.unit->parent_unit->type_interner.arena);
			for (auto field: d->struct_decl.fields)
			{
				auto field_type = _typer_resolve_type_sign(self, field.type);
				if (field_type->kind == Type::KIND_INCOMPLETE || field_type->kind == Type::KIND_COMPLETING)
					_typer_complete_type(self, field_type->struct_type.symbol, type_sign_location(field.type));
				for (auto name: field.names)
				{
					Field_Type struct_field{};
					struct_field.name = name;
					struct_field.type = field_type;
					mn::buf_push(struct_fields, struct_field);

					if (auto it = mn::map_lookup(struct_fields_by_name, name.str))
					{
						auto old_loc = struct_fields[it->value].name.loc;

						Err err{};
						err.loc = name.loc;
						err.msg = mn::strf("'{}' field redefinition, first declared in {}:{}", name.str, old_loc.pos.line, old_loc.pos.col);
						unit_err(self.unit, err);
					}
					else
					{
						mn::map_insert(struct_fields_by_name, name.str, struct_fields.count - 1);
					}
				}
			}
			type_interner_complete(self.unit->parent_unit->type_interner, type, struct_fields, struct_fields_by_name);
		}
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
		case Symbol::KIND_STRUCT:
			sym->type = type_interner_incomplete(self.unit->parent_unit->type_interner, sym);
			break;
		case Symbol::KIND_PACKAGE:
			sym->type = type_interner_package(self.unit->parent_unit->type_interner, sym->package_sym.package);
			break;
		case Symbol::KIND_FUNC_OVERLOAD_SET:
			sym->type = _typer_resolve_func_overload_set(self, sym);
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
		case Symbol::KIND_FUNC_OVERLOAD_SET:
			_typer_resolve_func_overload_set_body(self, sym);
			break;
		case Symbol::KIND_PACKAGE:
			unit_package_check(sym->package_sym.package);
			break;
		case Symbol::KIND_STRUCT:
			_typer_complete_type(self, sym, symbol_location(sym));
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
		auto compile_ifs = mn::buf_with_allocator<Decl*>(mn::memory::tmp());

		for (auto file: self.unit->files)
		{
			for (auto decl: file->decls)
			{
				if (decl->kind == Decl::KIND_IF)
					mn::buf_push(compile_ifs, decl);
				else
					_typer_shallow_process_decl(self, file, decl);
			}
		}

		for (size_t i = 0; i < compile_ifs.count; ++i)
		{
			auto if_decl = compile_ifs[i];
			size_t winner_if_index = if_decl->if_decl.cond.count;
			for (size_t j = 0; j < if_decl->if_decl.cond.count; ++j)
			{
				auto cond_expr = if_decl->if_decl.cond[j];
				auto cond_type = _typer_resolve_expr(self, cond_expr);
				if (cond_type != type_bool)
				{
					Err err{};
					err.loc = cond_expr->loc;
					err.msg = mn::strf("if condition type '{}' is not a boolean", cond_type);
					unit_err(self.unit, err);
				}

				if (cond_expr->mode != ADDRESS_MODE_CONST)
				{
					Err err{};
					err.loc = cond_expr->loc;
					err.msg = mn::strf("compile time if condition is not a constant");
					unit_err(self.unit, err);
				}

				if (cond_expr->const_value.kind == Expr_Value::KIND_BOOL && cond_expr->const_value.as_bool)
				{
					winner_if_index = j;
					break;
				}
			}

			if (winner_if_index < if_decl->if_decl.cond.count)
			{
				for (auto decl: if_decl->if_decl.body[winner_if_index])
				{
					if (decl->kind == Decl::KIND_IF)
						mn::buf_push(compile_ifs, decl);
					else
						_typer_shallow_process_decl(self, decl->loc.file, decl);
				}
			}
			else
			{
				for (auto decl: if_decl->if_decl.else_body)
				{
					if (decl->kind == Decl::KIND_IF)
						mn::buf_push(compile_ifs, decl);
					else
						_typer_shallow_process_decl(self, decl->loc.file, decl);
				}
			}
		}
	}

	// API
	Typer
	typer_new(Unit_Package* unit)
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