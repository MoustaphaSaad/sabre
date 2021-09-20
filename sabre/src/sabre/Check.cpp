#include "sabre/Check.h"
#include "sabre/Unit.h"
#include "sabre/Type_Interner.h"

#include <mn/IO.h>
#include <mn/Log.h>
#include <mn/Defer.h>

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
			const char* name = "symbol";
			if (sym->kind == Symbol::KIND_PACKAGE)
				name = "package";
			if (old_loc.pos.line > 0)
				err.msg = mn::strf("'{}' {} redefinition, first declared in {}:{}", sym->name, name, old_loc.pos.line, old_loc.pos.col);
			else
				err.msg = mn::strf("'{}' {} redefinition", sym->name, name);
			unit_err(self.unit, err);

			// just copy these values from the old symbol
			sym->package = old_sym->package;
			sym->scope = old_sym->scope;

			return old_sym;
		}
		scope_add(current_scope, sym);
		sym->package = self.unit;
		sym->scope = current_scope;
		return sym;
	}

	inline static Symbol*
	_typer_find_symbol(const Typer& self, const char* name)
	{
		return scope_find(_typer_current_scope(self), name);
	}

	inline static bool
	_typer_can_assign(Type* lhs, Expr* rhs)
	{
		// special case sampler + sampler state
		if (type_is_sampler(lhs))
		{
			if (type_is_sampler(rhs->type) || type_is_sampler_state(rhs->type))
				return true;
		}

		// if we have different types then we can't assign
		if (type_is_equal(lhs, rhs->type) == false)
			return false;

		if (rhs->type == type_lit_int)
		{
			if (lhs == type_int || lhs == type_lit_int)
			{
				return true;
			}
			else if (lhs == type_uint)
			{
				bool mismatch = false;
				if (rhs->mode == ADDRESS_MODE_CONST)
				{
					if (rhs->const_value.type == type_int)
					{
						mismatch = rhs->const_value.as_int < 0;
					}
					else if (rhs->const_value.type == type_double)
					{
						mismatch = rhs->const_value.as_double < 0;
					}
					else
					{
						mismatch = true;
					}
				}
				else
				{
					mismatch = true;
				}
				return mismatch == false;
			}
			else if (lhs == type_float || lhs == type_lit_float)
			{
				return true;
			}
			else if (lhs == type_double)
			{
				return true;
			}
			else
			{
				return false;
			}
		}
		else if (rhs->type == type_lit_float)
		{
			if (lhs == type_int || lhs == type_lit_int)
			{
				bool mismatch = false;
				if (rhs->mode == ADDRESS_MODE_CONST)
				{
					if (rhs->const_value.type == type_int)
					{
						mismatch = false;
					}
					else if (rhs->const_value.type == type_double)
					{
						mismatch = (rhs->const_value.as_double - (int64_t)rhs->const_value.as_double) != 0;
					}
					else
					{
						mismatch = true;
					}
				}
				else
				{
					mismatch = true;
				}
				return mismatch == false;
			}
			else if (lhs == type_uint)
			{
				bool mismatch = false;
				if (rhs->mode == ADDRESS_MODE_CONST)
				{
					if (rhs->const_value.type == type_int)
					{
						mismatch = rhs->const_value.as_int < 0;
					}
					else if (rhs->const_value.type == type_double)
					{
						mismatch = rhs->const_value.as_double < 0;
						mismatch |= (rhs->const_value.as_double - (int64_t)rhs->const_value.as_double) != 0;
					}
					else
					{
						mismatch = true;
					}
				}
				else
				{
					mismatch = true;
				}
				return mismatch == false;
			}
			else if (lhs == type_float || lhs == type_lit_float)
			{
				return true;
			}
			else if (lhs == type_double)
			{
				return true;
			}
			else
			{
				return false;
			}
		}
		else
		{
			return true;
		}
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
	_typer_shallow_walk(Typer& self);

	inline static void
	_typer_add_func_overload(Typer& self, Type* overload_set, Decl* decl)
	{
		Type_Overload_Entry overload_entry{};
		overload_entry.loc = decl->loc;
		overload_entry.type = _typer_resolve_func_decl(self, decl);

		if (auto it = mn::map_lookup(overload_set->func_overload_set_type.overloads, overload_entry.type->as_func.sign.args))
		{
			auto old_loc = it->value.loc;
			Err err{};
			err.loc = decl->loc;
			err.msg = mn::strf("function overload already defined {}:{}:{}", old_loc.file->filepath, old_loc.pos.line, old_loc.pos.col);
			unit_err(self.unit, err);
		}
		else
		{
			mn::map_insert(overload_set->func_overload_set_type.overloads, overload_entry.type->as_func.sign.args, overload_entry);
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
		{
			// add symbol twice, once in file scope an another one in package scope
			auto sym = symbol_func_new(self.unit->symbols_arena, decl->name, decl);
			auto res = _typer_add_symbol(self, sym);
			return res;
		}

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
		auto decl_type = type_void;
		if (sym->state == STATE_RESOLVED)
			decl_type = _typer_resolve_func_decl(self, decl);
		mn::map_insert(sym->func_overload_set_sym.decls, decl, decl_type);
		if (sym->state == STATE_RESOLVED)
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
				// add symbol twice, once in file scope an another one in package scope
				auto sym = symbol_const_new(self.unit->symbols_arena, name, decl, sign, value);
				_typer_add_symbol(self, sym);
				// search for the pipeline of that shader
				if (mn::map_lookup(decl->tags.table, KEYWORD_REFLECT))
					mn::buf_push(self.unit->parent_unit->reflected_symbols, sym);
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

				// add symbol twice, once in file scope an another one in package scope
				auto sym = symbol_var_new(self.unit->symbols_arena, name, decl, sign, value);
				_typer_add_symbol(self, sym);
			}
			break;
		case Decl::KIND_FUNC:
			_typer_add_func_symbol(self, decl);
			break;
		case Decl::KIND_STRUCT:
		{
			auto sym = symbol_struct_new(self.unit->symbols_arena, decl->name, decl);
			_typer_add_symbol(self, sym);
			break;
		}
		case Decl::KIND_IMPORT:
		{
			// TODO(Moustapha): unescape the string
			auto package_path = mn::str_from_c(decl->import_decl.path.str, mn::memory::tmp());
			mn::str_trim(package_path, "\"");

			auto [package, resolve_err] = unit_file_resolve_package(file, package_path);
			if (resolve_err == false)
			{
				Tkn name{};
				if (decl->import_decl.name)
					name = decl->import_decl.name;
				else
					name = package->name;
				auto sym = symbol_package_new(self.unit->symbols_arena, name, decl, package);
				// we put the import declarations into the file scope to enable users
				// to include the same library with the same name in different files of
				// the same folder package
				_typer_enter_scope(self, file->file_scope);
				auto added_sym = _typer_add_symbol(self, sym);
				_typer_leave_scope(self);

				if (added_sym != sym)
				{
					Err err{};
					err.loc = symbol_location(added_sym);
					if (added_sym->kind == Symbol::KIND_PACKAGE)
						err.msg = mn::strf("package '{}' was first imported here", added_sym->name);
					else
						err.msg = mn::strf("symbol '{}' was first imported here", added_sym->name);
					unit_err(self.unit, err);
					break;
				}

				// add symbol in global scope only once to avoid symbol redefinition and get symbol redefinition
				// detection between namespaces and other declaration
				if (auto old_sym = scope_shallow_find(self.global_scope, sym->name))
				{
					if (old_sym->kind != Symbol::KIND_PACKAGE || old_sym->package_sym.package != sym->package_sym.package)
					{
						_typer_add_symbol(self, sym);
					}
				}
				else
				{
					_typer_add_symbol(self, sym);
				}
			}
			break;
		}
		case Decl::KIND_ENUM:
		{
			auto sym = symbol_enum_new(self.unit->symbols_arena, decl->name, decl);
			_typer_add_symbol(self, sym);
			break;
		}
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_typer_push_expected_expression_type(Typer& self, Type* t)
	{
		mn::buf_push(self.expected_expr_type, t);
	}

	inline static void
	_typer_pop_expected_expression_type(Typer& self)
	{
		mn:buf_pop(self.expected_expr_type);
	}

	inline static Type*
	_typer_expected_expression_type(Typer& self)
	{
		if (self.expected_expr_type.count > 0)
			return mn::buf_top(self.expected_expr_type);
		else
			return nullptr;
	}

	inline static Type*
	_typer_peel_top_type(Type* t)
	{
		switch (t->kind)
		{
		case Type::KIND_VEC:
			return t->vec.base;
		case Type::KIND_ARRAY:
			return t->array.base;
		case Type::KIND_INCOMPLETE:
		case Type::KIND_COMPLETING:
		case Type::KIND_VOID:
		case Type::KIND_BOOL:
		case Type::KIND_INT:
		case Type::KIND_UINT:
		case Type::KIND_FLOAT:
		case Type::KIND_DOUBLE:
		case Type::KIND_FUNC:
		case Type::KIND_STRUCT:
		case Type::KIND_TEXTURE:
		case Type::KIND_PACKAGE:
		case Type::KIND_FUNC_OVERLOAD_SET:
		case Type::KIND_MAT:
		default:
			return nullptr;
		}
	}

	inline static Type*
	_typer_resolve_named_type_atom(Typer& self, const Type_Sign_Atom& atom)
	{
		Type* res = nullptr;

		// type from imported package
		if (atom.named.package_name)
		{
			// package sym
			auto package_sym = _typer_find_symbol(self, atom.named.package_name.str);
			// search for package import in the same file as usage
			if (package_sym == nullptr)
			{
				auto file_scope = atom.named.package_name.loc.file->file_scope;
				package_sym = scope_find(file_scope, atom.named.package_name.str);
			}

			if (package_sym == nullptr)
			{
				Err err{};
				err.loc = atom.named.package_name.loc;
				err.msg = mn::strf("'{}' undefined symbol", atom.named.package_name.str);
				unit_err(self.unit, err);
				return res;
			}

			if (package_sym->kind != Symbol::KIND_PACKAGE)
			{
				Err err{};
				err.loc = atom.named.package_name.loc;
				err.msg = mn::strf("'{}' is not an imported package", atom.named.package_name.str);
				unit_err(self.unit, err);
				return res;
			}

			// make sure the package is resolved before usage
			_typer_resolve_symbol(self, package_sym);

			auto package = package_sym->package_sym.package;
			auto type_symbol = scope_shallow_find(package->global_scope, atom.named.type_name.str);
			if (type_symbol == nullptr)
			{
				Err err{};
				err.loc = atom.named.type_name.loc;
				err.msg = mn::strf("'{}' undefined symbol", atom.named.type_name.str);
				unit_err(self.unit, err);
				return res;
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
		return res;
	}

	inline static Type*
	_typer_resolve_type_sign(Typer& self, const Type_Sign& sign);

	inline static Type*
	_typer_template_instantiate(Typer& self, Type* template_type, const Type_Sign_Atom& template_atom)
	{
		// we should do something with template arguments
		auto args_types = mn::buf_with_allocator<Type*>(mn::memory::tmp());
		mn::buf_reserve(args_types, template_atom.templated.args.count);
		for (const auto& arg_type_sign: template_atom.templated.args)
		{
			auto type = _typer_resolve_type_sign(self, arg_type_sign);
			assert(type_is_template_incomplete(type) == false);
			mn::buf_push(args_types, type);
		}

		switch (template_type->kind)
		{
		case Type::KIND_STRUCT:
		{
			if (template_type->struct_type.template_args.count == 0)
			{
				Err err{};
				err.loc = template_atom.templated.type_name.loc;
				err.msg = mn::strf(
					"type '{}' is not a template type",
					template_type
				);
				unit_err(self.unit, err);
				return template_type;
			}

			if (args_types.count != template_type->struct_type.template_args.count)
			{
				Err err{};
				err.loc = template_atom.templated.type_name.loc;
				err.msg = mn::strf(
					"template type expected #{} arguments, but #{} only was provided",
					template_type->struct_type.template_args.count,
					args_types.count
				);
				unit_err(self.unit, err);
				return template_type;
			}

			auto fields_types = mn::buf_with_allocator<Type*>(mn::memory::tmp());
			mn::buf_reserve(fields_types, template_type->struct_type.fields.count);

			for (auto& field: template_type->struct_type.fields)
			{
				if (type_is_template_incomplete(field.type) == false)
				{
					mn::buf_push(fields_types, field.type);
				}
				else
				{
					size_t template_args_index = template_type->struct_type.template_args.count;
					for (size_t i = 0; i < template_type->struct_type.template_args.count; ++i)
					{
						if (type_is_equal(field.type, template_type->struct_type.template_args[i]))
						{
							template_args_index = i;
							break;
						}
					}
					// TODO: revisit later, you probably should issue an error here
					assert(template_args_index < args_types.count);
					mn::buf_push(fields_types, args_types[template_args_index]);
				}
			}

			return type_interner_template_struct_instantiate(self.unit->parent_unit->type_interner, template_type, fields_types);
		}
		default:
			assert(false && "unreachable");
			return type_void;
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
			{
				if (auto named_type = _typer_resolve_named_type_atom(self, atom))
					res = named_type;
				break;
			}
			case Type_Sign_Atom::KIND_ARRAY:
			{
				if (atom.array.static_size)
				{
					auto array_count_type = _typer_resolve_expr(self, atom.array.static_size);
					if (type_is_equal(array_count_type, type_int) == false &&
						type_is_equal(array_count_type, type_uint))
					{
						Err err{};
						err.loc = atom.array.static_size->loc;
						err.msg = mn::strf("array count should be integer but found '{}'", array_count_type);
						unit_err(self.unit, err);
					}

					if (atom.array.static_size->const_value.type == type_int)
					{
						auto array_count = atom.array.static_size->const_value.as_int;
						if (array_count < 0)
						{
							Err err{};
							err.loc = atom.array.static_size->loc;
							err.msg = mn::strf("array count should be >= but found '{}'", array_count);
							unit_err(self.unit, err);
						}
						Array_Sign sign{};
						sign.base = res;
						sign.count = array_count;
						res = type_interner_array(self.unit->parent_unit->type_interner, sign);
					}
				}
				else
				{
					// we have a dynamically sized array
					// TODO(Moustapha): maybe add support for array slices later
					Array_Sign sign{};
					sign.base = res;
					sign.count = -1;
					res = type_interner_array(self.unit->parent_unit->type_interner, sign);
				}
				break;
			}
			case Type_Sign_Atom::KIND_TEMPLATED:
			{
				if (auto named_type = _typer_resolve_named_type_atom(self, atom))
					res = _typer_template_instantiate(self, named_type, atom);
				break;
			}
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
		switch (e->atom.tkn.kind)
		{
		case Tkn::KIND_LITERAL_INTEGER:
			e->mode = ADDRESS_MODE_CONST;
			e->const_value = expr_value_int(::strtoll(e->atom.tkn.str, nullptr, 10));
			return type_lit_int;
		case Tkn::KIND_LITERAL_FLOAT:
			e->mode = ADDRESS_MODE_CONST;
			e->const_value = expr_value_double(::strtod(e->atom.tkn.str, nullptr));
			return type_lit_float;
		case Tkn::KIND_KEYWORD_FALSE:
			e->mode = ADDRESS_MODE_CONST;
			e->const_value = expr_value_bool(false);
			return type_bool;
		case Tkn::KIND_KEYWORD_TRUE:
			e->mode = ADDRESS_MODE_CONST;
			e->const_value = expr_value_bool(true);
			return type_bool;
		case Tkn::KIND_ID:
		{
			// try to find the symbol in the current scope
			auto sym = _typer_find_symbol(self, e->atom.tkn.str);
			// if you don't find it then maybe it's an import so we have to search in file scope as well
			if (sym == nullptr)
			{
				auto file_scope = e->loc.file->file_scope;
				sym = scope_find(file_scope, e->atom.tkn.str);
			}

			if (sym)
			{
				e->atom.sym = sym;
				e->atom.decl = symbol_decl(sym);
				_typer_resolve_symbol(self, sym);
				if (sym->kind == Symbol::KIND_CONST && sym->const_sym.value != nullptr)
				{
					e->const_value = sym->const_sym.value->const_value;
				}

				if (sym->kind == Symbol::KIND_CONST)
					e->mode = ADDRESS_MODE_CONST;
				else if (sym->kind == Symbol::KIND_VAR)
					e->mode = ADDRESS_MODE_VARIABLE;
				else if (sym->kind == Symbol::KIND_FUNC && sym->type->as_func.sign.return_type != type_void)
					e->mode = ADDRESS_MODE_COMPUTED_VALUE;

				e->symbol = sym;
				return sym->type;
			}
			else
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("'{}' undefined symbol", e->atom.tkn.str);
				unit_err(self.unit, err);
				return type_void;
			}
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

		if (type_is_enum(lhs_type))
			_typer_push_expected_expression_type(self, lhs_type);

		auto rhs_type = _typer_resolve_expr(self, e->binary.right);

		if (type_is_enum(lhs_type))
			_typer_pop_expected_expression_type(self);

		bool failed = false;

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
					failed = true;
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
					failed = true;
				}
			}
		}

		// handle vector scalar operations
		if (e->binary.op.kind == Tkn::KIND_PLUS ||
			e->binary.op.kind == Tkn::KIND_MINUS ||
			e->binary.op.kind == Tkn::KIND_STAR ||
			e->binary.op.kind == Tkn::KIND_DIVIDE ||
			e->binary.op.kind == Tkn::KIND_MODULUS)
		{
			if (lhs_type->kind == Type::KIND_VEC && type_is_numeric_scalar(rhs_type))
			{
				if (type_is_equal(lhs_type->vec.base, rhs_type))
				{
					return lhs_type;
				}
				else
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("illegal binary operation on vector type, lhs is '{}' and rhs is '{}'", lhs_type, rhs_type);
					unit_err(self.unit, err);
					failed = true;
				}
			}
			else if (type_is_numeric_scalar(lhs_type) && rhs_type->kind == Type::KIND_VEC)
			{
				if (type_is_equal(rhs_type->vec.base, lhs_type))
				{
					return rhs_type;
				}
				else
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("illegal binary operation on vector type, lhs is '{}' and rhs is '{}'", lhs_type, rhs_type);
					unit_err(self.unit, err);
					failed = true;
				}
			}
		}

		if (e->binary.op.kind == Tkn::KIND_BIT_OR ||
			e->binary.op.kind == Tkn::KIND_BIT_AND ||
			e->binary.op.kind == Tkn::KIND_BIT_XOR)
		{
			if (type_has_bit_ops(lhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.left->loc;
				err.msg = mn::strf("type '{}' doesn't support bitwise operations", lhs_type);
				unit_err(self.unit, err);
			}

			if (type_has_bit_ops(rhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.right->loc;
				err.msg = mn::strf("type '{}' doesn't support bitwise operations", rhs_type);
				unit_err(self.unit, err);
			}
		}
		else if (e->binary.op.kind == Tkn::KIND_BIT_SHIFT_LEFT ||
				 e->binary.op.kind == Tkn::KIND_BIT_SHIFT_RIGHT)
		{
			if (type_has_bit_ops(lhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.left->loc;
				err.msg = mn::strf("type '{}' doesn't support bitwise operations", lhs_type);
				unit_err(self.unit, err);
			}

			if (type_has_bit_ops(rhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.right->loc;
				err.msg = mn::strf("type '{}' doesn't support bitwise operations", rhs_type);
				unit_err(self.unit, err);
			}
		}

		if (failed == false && type_is_equal(lhs_type, rhs_type) == false)
		{
			// TODO(Moustapha): better error message here, highlight parts of the expression with their types
			if (type_is_enum(lhs_type) && type_is_equal(rhs_type, type_int) ||
				type_is_enum(rhs_type) && type_is_equal(lhs_type, type_int))
			{
				// enum and int types can be used in a binary expression
			}
			else if (e->binary.op.kind == Tkn::KIND_BIT_SHIFT_LEFT ||
					 e->binary.op.kind == Tkn::KIND_BIT_SHIFT_RIGHT)
			{
				if (type_has_bit_ops(rhs_type) == false)
				{
					Err err{};
					err.loc = e->binary.right->loc;
					err.msg = mn::strf("type '{}' cannot be used in a bitwise shift operation", rhs_type);
					unit_err(self.unit, err);
				}
				else if (type_width(lhs_type) != type_width(rhs_type))
				{
					Err err{};
					err.loc = e->binary.right->loc;
					err.msg = mn::strf("type '{}' is not compatible with '{}' in a bitwise shift operation", lhs_type, rhs_type);
					unit_err(self.unit, err);
				}
			}
			else
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("type mismatch in binary expression, lhs is '{}' and rhs is '{}'", lhs_type, rhs_type);
				unit_err(self.unit, err);
			}
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
			e->const_value = expr_value_binary_op(e->binary.left->const_value, e->binary.op.kind, e->binary.right->const_value);
			e->mode = ADDRESS_MODE_CONST;
		}
		else
		{
			e->mode = ADDRESS_MODE_COMPUTED_VALUE;
		}

		if (tkn_is_cmp(e->binary.op.kind))
		{
			if (type_is_vec(lhs_type))
			{
				return type_vectorize(type_bool, lhs_type->vec.width);
			}
			else if (type_is_vec(rhs_type))
			{
				return type_vectorize(type_bool, rhs_type->vec.width);
			}
			else
			{
				return type_bool;
			}
		}

		if (lhs_type == type_lit_int || lhs_type == type_lit_float)
		{
			return rhs_type;
		}
		else
		{
			return lhs_type;
		}
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
		else if (e->unary.op.kind == Tkn::KIND_BIT_NOT)
		{
			if (type_has_bit_ops(type) == false)
			{
				Err err{};
				err.loc = e->unary.base->loc;
				err.msg = mn::strf("type '{}' cannot be used in a bit not operation", type);
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

		if (e->unary.base->const_value.type != nullptr)
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
			auto symbol = e->call.base->symbol;
			// special case emit function
			if (auto decl = symbol_decl(symbol))
			{
				if (mn::map_lookup(decl->tags.table, KEYWORD_GEOMETRY_EMIT_FUNC) != nullptr)
				{
					if (e->call.args.count != 1)
					{
						Err err{};
						err.loc = e->loc;
						err.msg = mn::strf("function expected {} arguments, but {} were provided", type->as_func.sign.args.types.count, e->call.args.count);
						unit_err(self.unit, err);
						return type->as_func.sign.return_type;
					}

					auto arg_type = _typer_resolve_expr(self, e->call.args[0]);
					if (auto out_sym = self.unit->parent_unit->geometry_shader_output)
					{
						_typer_resolve_symbol(self, out_sym);
						if (type_is_equal(out_sym->type, arg_type) == false)
						{
							Err err{};
							err.loc = e->call.args[0]->loc;
							err.msg = mn::strf("function argument #{} type mismatch, expected '{}' but found '{}'", 0, out_sym->type, arg_type);
							unit_err(self.unit, err);
						}
					}
					return type->as_func.sign.return_type;
				}
			}

			if (e->call.args.count != type->as_func.sign.args.types.count)
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("function expected {} arguments, but {} were provided", type->as_func.sign.args.types.count, e->call.args.count);
				unit_err(self.unit, err);
				return type->as_func.sign.return_type;
			}

			for (size_t i = 0; i < e->call.args.count; ++i)
			{
				auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
				if (_typer_can_assign(type->as_func.sign.args.types[i], e->call.args[i]) == false)
				{
					Err err{};
					err.loc = e->call.args[i]->loc;
					err.msg = mn::strf("function argument #{} type mismatch, expected '{}' but found '{}'", i, type->as_func.sign.args.types[i], arg_type);
					unit_err(self.unit, err);
				}
			}

			if (e->call.base->kind == Expr::KIND_ATOM)
			{
				e->call.func = e->call.base->atom.sym->func_sym.decl;
			}
			return type->as_func.sign.return_type;
		}
		else if (type->kind == Type::KIND_FUNC_OVERLOAD_SET)
		{
			auto overload_set_symbol = type->func_overload_set_type.symbol;
			Type* res = nullptr;
			for (auto& [overload_decl, overload_type]: overload_set_symbol->func_overload_set_sym.decls)
			{
				if (e->call.args.count != overload_type->as_func.sign.args.types.count)
					continue;

				bool args_match = true;
				for (size_t i = 0; i < e->call.args.count; ++i)
				{
					auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
					if (_typer_can_assign(overload_type->as_func.sign.args.types[i], e->call.args[i]) == false)
					{
						args_match = false;
						break;
					}
				}
				if (args_match)
				{
					if (e->call.base->kind == Expr::KIND_ATOM)
						e->call.base->atom.decl = overload_decl;
					e->call.func = overload_decl;
					res = overload_type->as_func.sign.return_type;
					if (mn::set_lookup(overload_set_symbol->func_overload_set_sym.unique_used_decls, overload_decl) == nullptr)
					{
						mn::buf_push(overload_set_symbol->func_overload_set_sym.used_decls, overload_decl);
						mn::set_insert(overload_set_symbol->func_overload_set_sym.unique_used_decls, overload_decl);
					}
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

		if (e->cast.base->const_value.type != nullptr)
			e->const_value = e->cast.base->const_value;

		auto res = to_type;
		if (type_is_numeric_scalar(from_type) && type_is_numeric_scalar(to_type))
		{
			res = to_type;
		}
		else if (from_type->kind == Type::KIND_VEC &&
				 to_type->kind == Type::KIND_VEC &&
				 from_type->vec.width == to_type->vec.width &&
				 type_is_numeric_scalar(from_type->vec.base) &&
				 type_is_numeric_scalar(to_type->vec.base))
		{
			res = to_type;
		}
		else if ((type_is_enum(from_type) && type_is_numeric_scalar(to_type)) ||
				 (type_is_numeric_scalar(from_type) && type_is_enum(to_type)))
		{
			res = to_type;
		}
		else
		{
			Err err{};
			err.loc = e->loc;
			err.msg = mn::strf("cannot cast '{}' to '{}'", from_type, to_type);
			unit_err(self.unit, err);
		}

		if (e->cast.base->mode == ADDRESS_MODE_CONST)
		{
			e->const_value = e->cast.base->const_value;
			e->mode = ADDRESS_MODE_CONST;
		}
		else
		{
			e->mode = e->cast.base->mode;
		}

		return res;
	}

	inline static Type*
	_typer_resolve_dot_expr(Typer& self, Expr* e)
	{
		Type* type = type_void;
		if (e->dot.lhs)
			type = _typer_resolve_expr(self, e->dot.lhs);
		else
			type = _typer_expected_expression_type(self);

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

			auto it = e->dot.rhs->atom.tkn.str;
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

			auto it = mn::map_lookup(type->struct_type.fields_by_name, e->dot.rhs->atom.tkn.str);
			if (it == nullptr)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("unknown structure field");
				unit_err(self.unit, err);
				return type_void;
			}

			e->mode = e->dot.lhs->mode;
			e->symbol = type->struct_type.symbol;
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
			auto symbol = scope_shallow_find(package->global_scope, e->dot.rhs->atom.tkn.str);
			if (symbol == nullptr)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("undefined symbol");
				unit_err(self.unit, err);
				return type_void;
			}

			if (symbol->kind == Symbol::KIND_PACKAGE)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("you can't import a package from inside another package");
				unit_err(self.unit, err);
			}

			e->dot.rhs->atom.sym = symbol;
			e->dot.rhs->atom.decl = symbol_decl(symbol);
			_typer_resolve_symbol(self, symbol);
			e->symbol = symbol;
			return symbol->type;
		}
		else if (type->kind == Type::KIND_ENUM)
		{
			if (e->dot.rhs->kind != Expr::KIND_ATOM)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("unknown structure field");
				unit_err(self.unit, err);
				return type_void;
			}

			auto it = mn::map_lookup(type->enum_type.fields_by_name, e->dot.rhs->atom.tkn.str);
			if (it == nullptr)
			{
				Err err{};
				err.loc = e->dot.rhs->loc;
				err.msg = mn::strf("unknown enum field");
				unit_err(self.unit, err);
				return type_void;
			}

			auto value = type->enum_type.fields[it->value].value;
			if (value.type != nullptr)
			{
				e->mode = ADDRESS_MODE_CONST;
				e->const_value = value;
			}
			else
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("enum field has no value yet");
				unit_err(self.unit, err);
			}
			e->symbol = type->enum_type.symbol;
			return type;
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
		auto base_type = _typer_resolve_expr(self, e->indexed.base);
		if (type_is_array(base_type) == false)
		{
			Err err{};
			err.loc = e->loc;
			err.msg = mn::strf("type '{}' is not array", base_type);
			unit_err(self.unit, err);
			return base_type;
		}

		auto index_type = _typer_resolve_expr(self, e->indexed.index);
		if (type_is_equal(index_type, type_int) == false &&
			type_is_equal(index_type, type_uint) == false)
		{
			Err err{};
			err.loc = e->indexed.index->loc;
			err.msg = mn::strf("array index type should be an int or uint, but we found '{}'", index_type);
			unit_err(self.unit, err);
			return base_type->array.base;
		}

		if (e->indexed.index->mode == ADDRESS_MODE_CONST &&
			e->indexed.index->const_value.type == type_int &&
			e->indexed.index->const_value.as_int >= base_type->array.count)
		{
			Err err{};
			err.loc = e->indexed.index->loc;
			err.msg = mn::strf(
				"array index out of range, array count is '{}' but index is '{}'",
				base_type->array.count,
				e->indexed.index->const_value.as_int
			);
			unit_err(self.unit, err);
		}

		// arrays have variable mode by default, unless they are constants
		e->mode = ADDRESS_MODE_VARIABLE;
		if (e->indexed.base->mode == ADDRESS_MODE_CONST &&
			e->indexed.index->mode == ADDRESS_MODE_CONST)
		{
			if ((e->indexed.base->const_value.type && type_is_array(e->indexed.base->const_value.type)) &&
				e->indexed.index->const_value.type == type_int)
			{
				if (e->indexed.index->const_value.as_int < e->indexed.base->type->array.count)
				{
					e->mode = ADDRESS_MODE_CONST;
					e->const_value = expr_value_aggregate_get(e->indexed.base->const_value, e->indexed.index->const_value.as_int);
				}
			}
		}

		return base_type->array.base;
	}

	inline static Type*
	_typer_resolve_complit_expr(Typer& self, Expr* e)
	{
		Type* type = type_void;
		if (e->complit.type.atoms.count > 0)
		{
			type = _typer_resolve_type_sign(self, e->complit.type);
		}
		else
		{
			if (auto expected_type = _typer_expected_expression_type(self))
			{
				type = expected_type;
			}
			else
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("could not infer composite literal type");
				unit_err(self.unit, err);
			}
		}

		bool is_const = true;
		size_t type_field_index = 0;
		for (size_t i = 0; i < e->complit.fields.count; ++i)
		{
			auto& field = e->complit.fields[i];

			auto type_it = type;
			bool failed = false;
			if (field.selector_name)
			{
				if (type_it->kind == Type::KIND_VEC)
				{
					auto name = mn::str_lit(field.selector_name->atom.tkn.str);
					if (type_it->vec.width > 0 && name == "x")
					{
						field.selector_index = 0;
						type_it = type_it->vec.base;
					}
					else if (type_it->vec.width > 1 && name == "y")
					{
						field.selector_index = 1;
						type_it = type_it->vec.base;
					}
					else if (type_it->vec.width > 2 && name == "z")
					{
						field.selector_index = 2;
						type_it = type_it->vec.base;
					}
					else if (type_it->vec.width > 3 && name == "w")
					{
						field.selector_index = 3;
						type_it = type_it->vec.base;
					}
					else
					{
						Err err{};
						err.loc = field.selector_name->loc;
						err.msg = mn::strf("type '{}' doesn't have field '{}'", type_it, field.selector_name->atom.tkn.str);
						unit_err(self.unit, err);
						failed = true;
						break;
					}
				}
				else if (type_it->kind == Type::KIND_STRUCT)
				{
					auto field_it = mn::map_lookup(type_it->struct_type.fields_by_name, field.selector_name->atom.tkn.str);
					if (field_it == nullptr)
					{
						Err err{};
						err.loc = field.selector_name->loc;
						err.msg = mn::strf("type '{}' doesn't have field '{}'", type_it, field.selector_name->atom.tkn.str);
						unit_err(self.unit, err);
						failed = true;
						break;
					}
					field.selector_index = field_it->value;
					type_it = type_it->struct_type.fields[field_it->value].type;
				}
				else
				{
					Err err{};
					err.loc = field.selector_name->loc;
					err.msg = mn::strf("type '{}' doesn't have field '{}'", type_it, field.selector_name->atom.tkn.str);
					unit_err(self.unit, err);
					failed = true;
					break;
				}
			}
			else
			{
				if (type_it->kind == Type::KIND_VEC)
				{
					if (type_field_index < type_it->vec.width)
					{
						type_it = type_it->vec.base;
						field.selector_index = type_field_index;
						++type_field_index;
					}
					else
					{
						Err err{};
						err.loc = field.value->loc;
						err.msg = mn::strf("type '{}' contains only {} fields", type_it, type_it->vec.width);
						unit_err(self.unit, err);
						failed = true;
					}
				}
				else if (type_it->kind == Type::KIND_STRUCT)
				{
					if (type_field_index < type_it->struct_type.fields.count)
					{
						type_it = type_it->struct_type.fields[type_field_index].type;
						field.selector_index = type_field_index;
						++type_field_index;
					}
					else
					{
						Err err{};
						err.loc = field.value->loc;
						err.msg = mn::strf("type '{}' contains only {} fields", type_it, type_it->struct_type.fields.count);
						unit_err(self.unit, err);
						failed = true;
					}
				}
				else if (type_it->kind == Type::KIND_ARRAY)
				{
					// array count can be -1, to indicate an array which we don't know the size of yet
					if (type_field_index < type_it->array.count)
					{
						type_it = type_it->array.base;
						field.selector_index = type_field_index;
						++type_field_index;
					}
					else
					{
						Err err{};
						err.loc = field.value->loc;
						err.msg = mn::strf("array '{}' contains only {} elements", type_it, type_it->array.count);
						unit_err(self.unit, err);
						failed = true;
					}
				}
				else
				{
					Err err{};
					err.loc = field.value->loc;
					err.msg = mn::strf("type '{}' doesn't have fields", type_it);
					unit_err(self.unit, err);
					failed = true;
				}
			}

			if (failed == false)
			{
				if (auto it = mn::map_lookup(e->complit.referenced_fields, field.selector_index))
				{
					Err err{};
					err.loc = field.selector_name->loc;
					err.msg = mn::strf(
						"duplicate field name '{}' in composite literal",
						field.selector_name->atom.tkn.str
					);
					unit_err(self.unit, err);
				}
				else
				{
					mn::map_insert(e->complit.referenced_fields, field.selector_index, i);
				}
			}

			Type* expected_type = nullptr;
			if (field.selector_name && failed == false)
				expected_type = type_it;
			else
				expected_type = _typer_peel_top_type(type);

			if (expected_type != nullptr)
				_typer_push_expected_expression_type(self, expected_type);

			auto value_type = _typer_resolve_expr(self, field.value);

			if (expected_type != nullptr)
				_typer_pop_expected_expression_type(self);

			is_const &= field.value->mode == ADDRESS_MODE_CONST && field.value->const_value.type != nullptr;
			if (failed == false)
			{
				// special case vector upcast
				if (field.selector_name == nullptr && type->kind == Type::KIND_VEC && value_type->kind == Type::KIND_VEC)
				{
					if (value_type->vec.width <= type->vec.width && type_is_equal(value_type->vec.base, type->vec.base))
					{
						type_field_index += value_type->vec.width - 1;
					}
					else
					{
						Err err{};
						err.loc = field.value->loc;
						err.msg = mn::strf("type mismatch in compound literal value, type '{}' cannot be constructed from '{}'", type, value_type);
						unit_err(self.unit, err);
						break;
					}
				}
				else if (type_is_unbounded_array(type_it) && type_is_bounded_array(value_type))
				{
					// okay we can assign bounded arrays into unbounded ones because we are transferring
					// the size down in the code
					if (type_is_array(type) && type_is_unbounded_array(type->array.base))
					{
						type->array.base = value_type;
					}
				}
				else if (_typer_can_assign(type_it, field.value) == false)
				{
					Err err{};
					err.loc = field.value->loc;
					err.msg = mn::strf("type mismatch in compound literal value, selector type '{}' but expression type is '{}'", type_it, value_type);
					unit_err(self.unit, err);
					break;
				}
			}
		}

		// if this is an array with unknown size we set the size according to the number of elements in complit
		if (type_is_unbounded_array(type))
		{
			Array_Sign sign{};
			sign.base = type->array.base;
			sign.count = type_field_index;
			type = type_interner_array(self.unit->parent_unit->type_interner, sign);
		}

		// if all the field values are constant we'll consider the entire complit to be constant
		if (is_const)
		{
			// we currently handle arrays only
			if (type_is_vec(type))
			{
				e->const_value = expr_value_aggregate(e->loc.file->ast_arena, type);
				for (size_t i = 0; i < e->complit.fields.count; ++i)
				{
					auto field = e->complit.fields[i];
					expr_value_aggregate_set(e->const_value, field.selector_index, field.value->const_value);
				}

				e->mode = ADDRESS_MODE_CONST;
			}
			else if (type_is_array(type))
			{
				e->const_value = expr_value_aggregate(e->loc.file->ast_arena, type);
				for (size_t i = 0; i < e->complit.fields.count; ++i)
				{
					auto field = e->complit.fields[i];
					expr_value_aggregate_set(e->const_value, field.selector_index, field.value->const_value);
				}

				e->mode = ADDRESS_MODE_CONST;
			}
			else if (type_is_struct(type))
			{
				e->const_value = expr_value_aggregate(e->loc.file->ast_arena, type);
				for (size_t i = 0; i < e->complit.fields.count; ++i)
				{
					auto field = e->complit.fields[i];
					expr_value_aggregate_set(e->const_value, field.selector_index, field.value->const_value);
				}

				e->mode = ADDRESS_MODE_CONST;
			}
			else
			{
				// TODO(Moustapha): handle arbitrary constant types later
				// we need to have distinction between expression being const and having a constant value
				// for example `const x = 1.0` is a constant and the expression value should be a constant
				// while `var x = 1.0` is not a constant but it has a constant value, if we need to exploit
				// such things we'll need to have data flow analysis to ensure that x is not being changed
				// after the constant assignment and in this case we can treat it as const
				// assert(false && "only constant arrays are handled now");
			}
		}

		return type;
	}

	inline static Type*
	_typer_resolve_expr(Typer& self, Expr* e)
	{
		if (e->type)
			return e->type;

		switch (e->kind)
		{
		case Expr::KIND_ATOM:
			e->type = _typer_resolve_atom_expr(self, e);
			break;
		case Expr::KIND_BINARY:
			e->type = _typer_resolve_binary_expr(self, e);
			break;
		case Expr::KIND_UNARY:
			e->type = _typer_resolve_unary_expr(self, e);
			break;
		case Expr::KIND_CALL:
			e->type = _typer_resolve_call_expr(self, e);
			break;
		case Expr::KIND_CAST:
			e->type = _typer_resolve_cast_expr(self, e);
			break;
		case Expr::KIND_DOT:
			e->type = _typer_resolve_dot_expr(self, e);
			break;
		case Expr::KIND_INDEXED:
			e->type = _typer_resolve_indexed_expr(self, e);
			break;
		case Expr::KIND_COMPLIT:
			e->type = _typer_resolve_complit_expr(self, e);
			break;
		default:
			assert(false && "unreachable");
			e->type = type_void;
			break;
		}

		return e->type;
	}

	inline static Type*
	_typer_resolve_const(Typer& self, Symbol* sym)
	{
		// we should infer if the declaration has no type signature
		auto infer = sym->const_sym.sign.atoms.count == 0;

		auto res = type_void;
		Type* expected_type = nullptr;
		if (infer == false)
		{
			res = _typer_resolve_type_sign(self, sym->const_sym.sign);
			expected_type = res;
		}

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
				if (expected_type)
					_typer_push_expected_expression_type(self, expected_type);

				auto expr_type = _typer_resolve_expr(self, e);

				if (expected_type)
					_typer_pop_expected_expression_type(self);

				// check if left handside is an unknown array and complete it from the rhs
				if (type_is_unbounded_array(res) &&
					type_is_bounded_array(expr_type) &&
					type_is_equal(res->array.base, expr_type->array.base))
				{
					res = expr_type;
				}

				if (type_is_equal(expr_type, res) == false)
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("type mismatch expected '{}' but found '{}'", res, expr_type);
					unit_err(self.unit, err);
				}
			}
		}

		if (e && e->const_value.type == nullptr)
		{
			Err err{};
			err.loc = e->loc;
			err.msg = mn::strf("expression cannot be evaluated in compile time");
			unit_err(self.unit, err);
		}

		sym->type = res;
		return res;
	}

	inline static bool
	_typer_check_type_suitable_for_uniform(Typer& self, Type* type, size_t depth)
	{
		if (type_is_sampler(type))
		{
			return depth == 0;
		}
		else if (type->kind == Type::KIND_TEXTURE)
		{
			return depth == 0;
		}
		else if (type_is_struct(type))
		{
			bool res = true;
			for (auto field: type->struct_type.fields)
			{
				bool field_res = _typer_check_type_suitable_for_uniform(self, field.type, depth + 1);
				res &= field_res;

				if (field_res == false)
				{
					Err err{};
					err.loc = field.name.loc;
					err.msg = mn::strf("field type '{}' cannot be used for uniform", field.type);
					unit_err(self.unit, err);
				}
			}
			return res;
		}
		else if (type_is_unbounded_array(type))
		{
			Err err{};
			err.msg = mn::strf("'{}' unbounded arrays cannot be used in uniforms", type);
			unit_err(self.unit, err);
			return false;
		}
		else if (type_is_bounded_array(type))
		{
			return _typer_check_type_suitable_for_uniform(self, type->array.base, depth + 1);
		}
		else
		{
			return type_is_uniform(type);
		}
	}

	inline static Type*
	_typer_resolve_var(Typer& self, Symbol* sym)
	{
		// we should infer if the declaration has no type signature
		auto infer = sym->var_sym.sign.atoms.count == 0;

		auto res = type_void;
		Type* expected_type = nullptr;
		if (infer == false)
		{
			res = _typer_resolve_type_sign(self, sym->var_sym.sign);
			expected_type = res;
		}

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
				if (expected_type)
					_typer_push_expected_expression_type(self, expected_type);

				auto expr_type = _typer_resolve_expr(self, e);

				if (expected_type)
					_typer_pop_expected_expression_type(self);

				// check if left handside is an unknown array and complete it from the rhs
				if (type_is_unbounded_array(res) &&
					type_is_bounded_array(expr_type) &&
					type_is_equal(res->array.base, expr_type->array.base))
				{
					res = expr_type;
				}

				if (_typer_can_assign(res, e) == false)
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("type mismatch expected '{}' but found '{}'", res, expr_type);
					unit_err(self.unit, err);
				}
			}
		}
		sym->type = res;

		// check uniform types
		auto decl = symbol_decl(sym);
		if (auto uniform_tag_it = mn::map_lookup(decl->tags.table, KEYWORD_UNIFORM))
		{
			if (_typer_check_type_suitable_for_uniform(self, res, 0) == false)
			{
				Err err{};
				err.loc = symbol_location(sym);
				err.msg = mn::strf("uniform variable type '{}' contains types which cannot be used in a uniform", res);
				unit_err(self.unit, err);
			}

			if (sym->type->kind == Type::KIND_TEXTURE)
			{
				sym->var_sym.is_uniform = true;
				if (auto binding_it = mn::map_lookup(uniform_tag_it->value.args, KEYWORD_BINDING))
				{
					auto value_tkn = binding_it->value.value;
					if (value_tkn.kind == Tkn::KIND_LITERAL_INTEGER)
					{
						sym->var_sym.uniform_binding = ::atoi(value_tkn.str);
						if (sym->var_sym.uniform_binding > self.texture_binding_generator)
							self.texture_binding_generator = sym->var_sym.uniform_binding + 1;
					}
				}
				else
				{
					sym->var_sym.uniform_binding = self.texture_binding_generator++;
				}

				if (auto it = mn::map_lookup(self.unit->parent_unit->reachable_textures, sym->var_sym.uniform_binding))
				{
					auto old_sym = it->value;
					auto old_loc = symbol_location(old_sym);

					Err err{};
					err.loc = symbol_location(sym);
					err.msg = mn::strf(
						"texture binding point {} is shared with other texture defined in {}:{}",
						sym->var_sym.uniform_binding,
						old_loc.file->filepath,
						old_loc.pos.line
					);
					unit_err(self.unit, err);
				}
				else
				{
					mn::map_insert(self.unit->parent_unit->reachable_textures, sym->var_sym.uniform_binding, sym);
				}
			}
			else if (type_is_sampler(sym->type))
			{
				sym->var_sym.is_uniform = true;
				if (auto binding_it = mn::map_lookup(uniform_tag_it->value.args, KEYWORD_BINDING))
				{
					auto value_tkn = binding_it->value.value;
					if (value_tkn.kind == Tkn::KIND_LITERAL_INTEGER)
					{
						sym->var_sym.uniform_binding = ::atoi(value_tkn.str);
						if (sym->var_sym.uniform_binding > self.sampler_binding_generator)
							self.sampler_binding_generator = sym->var_sym.uniform_binding + 1;
					}
				}
				else
				{
					sym->var_sym.uniform_binding = self.sampler_binding_generator++;
				}

				if (auto it = mn::map_lookup(self.unit->parent_unit->reachable_samplers, sym->var_sym.uniform_binding))
				{
					auto old_sym = it->value;
					auto old_loc = symbol_location(old_sym);

					Err err{};
					err.loc = symbol_location(sym);
					err.msg = mn::strf(
						"sampler binding point {} is shared with other sampler defined in {}:{}",
						sym->var_sym.uniform_binding,
						old_loc.file->filepath,
						old_loc.pos.line
					);
					unit_err(self.unit, err);
				}
				else
				{
					mn::map_insert(self.unit->parent_unit->reachable_samplers, sym->var_sym.uniform_binding, sym);
				}
			}
			else
			{
				sym->var_sym.is_uniform = true;
				if (auto binding_it = mn::map_lookup(uniform_tag_it->value.args, KEYWORD_BINDING))
				{
					auto value_tkn = binding_it->value.value;
					if (value_tkn.kind == Tkn::KIND_LITERAL_INTEGER)
					{
						sym->var_sym.uniform_binding = ::atoi(value_tkn.str);
						if (sym->var_sym.uniform_binding > self.uniform_binding_generator)
							self.uniform_binding_generator = sym->var_sym.uniform_binding + 1;
					}
				}
				else
				{
					sym->var_sym.uniform_binding = self.uniform_binding_generator++;
				}

				if (auto it = mn::map_lookup(self.unit->parent_unit->reachable_uniforms, sym->var_sym.uniform_binding))
				{
					auto old_sym = it->value;
					auto old_loc = symbol_location(old_sym);

					Err err{};
					err.loc = symbol_location(sym);
					err.msg = mn::strf(
						"uniform binding point {} is shared with other uniform defined in {}:{}",
						sym->var_sym.uniform_binding,
						old_loc.file->filepath,
						old_loc.pos.line
					);
					unit_err(self.unit, err);
				}
				else
				{
					mn::map_insert(self.unit->parent_unit->reachable_uniforms, sym->var_sym.uniform_binding, sym);
				}
			}
		}

		return res;
	}

	inline static Type*
	_typer_resolve_func_decl(Typer& self, Decl* d)
	{
		// if we have calculated the type of the function then just return it
		if (d->type)
			return d->type;

		// TODO: find a nice way to handle the return type of function return type here, for now
		// we set it to void then overwrite it later at the end of this function
		auto scope = unit_create_scope_for(self.unit, d, _typer_current_scope(self), d->name.str, type_void, Scope::FLAG_NONE);
		_typer_enter_scope(self, scope);
		{
			auto type_interner = self.unit->parent_unit->type_interner;
			auto template_args = mn::buf_with_allocator<Type*>(type_interner->arena);
			for (auto template_arg: d->func_decl.template_args)
			{
				for (auto name: template_arg.names)
				{
					auto v = symbol_typename_new(self.unit->symbols_arena, name);
					auto type = type_interner_typename(type_interner, v);
					v->type = type;
					_typer_add_symbol(self, v);
					mn::buf_push(template_args, v->type);
				}
			}

			auto sign = func_sign_new();
			for (auto arg: d->func_decl.args)
			{
				auto arg_type = _typer_resolve_type_sign(self, arg.type);
				if (arg.names.count > 0)
				{
					for (size_t i = 0; i < arg.names.count; ++i)
						mn::buf_push(sign.args.types, arg_type);
				}
				else
				{
					mn::buf_push(sign.args.types, arg_type);
				}
			}
			sign.return_type = _typer_resolve_type_sign(self, d->func_decl.return_type);
			d->type = type_interner_func(self.unit->parent_unit->type_interner, sign, template_args);

			scope->expected_type = d->type->as_func.sign.return_type;

			// push function arguments into scope
			size_t i = 0;
			for (auto arg: d->func_decl.args)
			{
				auto arg_type = d->type->as_func.sign.args.types[i];
				for (auto name: arg.names)
				{
					auto v = symbol_var_new(self.unit->symbols_arena, name, nullptr, arg.type, nullptr);
					v->type = arg_type;
					v->state = STATE_RESOLVED;
					_typer_add_symbol(self, v);
					++i;
				}
			}
		}
		_typer_leave_scope(self);

		return d->type;
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
			// enter file scope to make import symbols visible
			_typer_enter_scope(self, decl->loc.file->file_scope);
			mn_defer(_typer_leave_scope(self));

			decl_type = _typer_resolve_func_decl(self, decl);
			_typer_add_func_overload(self, type, decl);
		}

		// TODO(Moustapha): check for duplicate function overloads
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
	_typer_resolve_discard_stmt(Typer& self, Stmt* s)
	{
		return type_void;
	}

	inline static Type*
	_typer_resolve_return_stmt(Typer& self, Stmt* s)
	{
		auto expected = _typer_expected_return_type(self);

		_typer_push_expected_expression_type(self, expected);
		auto ret = _typer_resolve_expr(self, s->return_stmt);
		_typer_pop_expected_expression_type(self);

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

			if (_typer_can_assign(lhs_type, s->assign_stmt.rhs[i]) == false)
			{
				// special case some of the operations
				if (s->assign_stmt.op.kind == Tkn::KIND_BIT_SHIFT_LEFT_EQUAL ||
					s->assign_stmt.op.kind == Tkn::KIND_BIT_SHIFT_RIGHT_EQUAL)
				{
					if (type_has_bit_ops(rhs_type) == false)
					{
						Err err{};
						err.loc = s->assign_stmt.rhs[i]->loc;
						err.msg = mn::strf("type '{}' cannot be used in a bitwise shift operation", rhs_type);
						unit_err(self.unit, err);
					}
					else if (type_width(lhs_type) != type_width(rhs_type))
					{
						Err err{};
						err.loc = s->assign_stmt.rhs[i]->loc;
						err.msg = mn::strf("type '{}' is not compatible with '{}' in a bitwise shift operation", lhs_type, rhs_type);
						unit_err(self.unit, err);
					}
				}
				else
				{
					Err err{};
					err.loc = s->assign_stmt.rhs[i]->loc;
					err.msg = mn::strf("type mismatch in assignment statement, expected '{}' but found '{}'", lhs_type, rhs_type);
					unit_err(self.unit, err);
				}
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
			{
				Err err{};
				err.loc = s->assign_stmt.lhs[i]->loc;
				err.msg = mn::strf("you can only assign into variables");
				unit_err(self.unit, err);
				break;
			}
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
				_typer_add_symbol(self, sym);
				_typer_resolve_symbol(self, sym);
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
				_typer_add_symbol(self, sym);
				_typer_resolve_symbol(self, sym);
			}
			break;
		case Decl::KIND_FUNC:
		{
			auto sym = _typer_add_func_symbol(self, d);
			_typer_add_symbol(self, sym);
			_typer_resolve_symbol(self, sym);
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
		case Stmt::KIND_DISCARD:
			return _typer_resolve_discard_stmt(self, s);
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
		if (type_is_templated(t))
			return;

		auto scope = unit_create_scope_for(self.unit, d, _typer_current_scope(self), d->name.str, t->as_func.sign.return_type, Scope::FLAG_NONE);
		_typer_enter_scope(self, scope);
		{
			// typecheck function body if it exists
			if (d->func_decl.body != nullptr)
			{
				for (auto stmt: d->func_decl.body->block_stmt)
					_typer_resolve_stmt(self, stmt);

				if (type_is_equal(t->as_func.sign.return_type, type_void) == false)
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

			auto scope = unit_create_scope_for(self.unit, d, _typer_current_scope(self), d->name.str, type_void, Scope::FLAG_NONE);
			_typer_enter_scope(self, scope);
			{
				auto type_interner = self.unit->parent_unit->type_interner;
				auto template_args = mn::buf_with_allocator<Type*>(type_interner->arena);
				for (auto template_arg: d->struct_decl.template_args)
				{
					for (auto name: template_arg.names)
					{
						auto v = symbol_typename_new(self.unit->symbols_arena, name);
						auto type = type_interner_typename(type_interner, v);
						v->type = type;
						_typer_add_symbol(self, v);
						mn::buf_push(template_args, v->type);
					}
				}

				auto struct_fields = mn::buf_with_allocator<Struct_Field_Type>(self.unit->parent_unit->type_interner->arena);
				auto struct_fields_by_name = mn::map_with_allocator<const char*, size_t>(self.unit->parent_unit->type_interner->arena);
				for (auto field: d->struct_decl.fields)
				{
					auto field_type = _typer_resolve_type_sign(self, field.type);
					if (field_type->kind == Type::KIND_INCOMPLETE || field_type->kind == Type::KIND_COMPLETING)
						_typer_complete_type(self, field_type->struct_type.symbol, type_sign_location(field.type));

					if (field.default_value)
					{
						_typer_push_expected_expression_type(self, field_type);
						auto default_value_type = _typer_resolve_expr(self, field.default_value);
						_typer_pop_expected_expression_type(self);

						if (type_is_equal(default_value_type, field_type) == false)
						{
							Err err{};
							err.loc = field.default_value->loc;
							err.msg = mn::strf("type mismatch in default value which has type '{}' but field type is '{}'", default_value_type, field_type);
							unit_err(self.unit, err);
						}

						if (field.default_value->mode != ADDRESS_MODE_CONST)
						{
							Err err{};
							err.loc = field.default_value->loc;
							err.msg = mn::strf("default value should be a constant");
							unit_err(self.unit, err);
						}
					}
					for (auto name: field.names)
					{
						Struct_Field_Type struct_field{};
						struct_field.name = name;
						struct_field.type = field_type;
						struct_field.default_value = field.default_value;
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
				type_interner_complete_struct(self.unit->parent_unit->type_interner, type, struct_fields, struct_fields_by_name, template_args);
			}
			_typer_leave_scope(self);
		}
		else if (sym->kind == Symbol::KIND_ENUM)
		{
			auto d = sym->enum_sym.decl;
			// first complete the type
			auto enum_fields = mn::buf_with_allocator<Enum_Field_Type>(self.unit->parent_unit->type_interner->arena);
			auto enum_fields_by_name = mn::map_with_allocator<const char*, size_t>(self.unit->parent_unit->type_interner->arena);
			for (auto field: d->enum_decl.fields)
			{
				Enum_Field_Type enum_field{};
				enum_field.name = field.name;
				mn::buf_push(enum_fields, enum_field);

				if (auto it = mn::map_lookup(enum_fields_by_name, field.name.str))
				{
					auto old_loc = enum_fields[it->value].name.loc;

					Err err{};
					err.loc = field.name.loc;
					err.msg = mn::strf("'{}' field redefinition, first declared in {}:{}", field.name.str, old_loc.pos.line, old_loc.pos.col);
					unit_err(self.unit, err);
				}
				else
				{
					mn::map_insert(enum_fields_by_name, field.name.str, enum_fields.count - 1);
				}
			}
			type_interner_complete_enum(self.unit->parent_unit->type_interner, type, enum_fields, enum_fields_by_name);

			// then fill the values
			auto enum_value = expr_value_int(0);
			for (size_t i = 0; i < d->enum_decl.fields.count; ++i)
			{
				const auto& decl_field = d->enum_decl.fields[i];
				if (decl_field.value)
				{
					_typer_push_expected_expression_type(self, type);
					auto value_type = _typer_resolve_expr(self, decl_field.value);
					_typer_pop_expected_expression_type(self);

					if (value_type != type && type_is_equal(value_type, type_int) == false)
					{
						Err err{};
						err.loc = decl_field.value->loc;
						err.msg = mn::strf("enum type should be integer, but instead we found '{}'", value_type);
						unit_err(self.unit, err);
						continue;
					}

					if (decl_field.value->mode != ADDRESS_MODE_CONST)
					{
						Err err{};
						err.loc = decl_field.value->loc;
						err.msg = mn::strf("enum values should be constant");
						unit_err(self.unit, err);
					}

					enum_value = decl_field.value->const_value;
				}

				type->enum_type.fields[i].value = enum_value;

				++enum_value.as_int;
			}
		}
	}

	inline static const char*
	_typer_generate_package_name_for_symbol(Typer& self, Symbol* sym, bool prepend_scope)
	{
		auto scope = sym->scope;

		auto res = mn::str_tmp();

		if (prepend_scope)
		{
			// we want to generate the name in reverse order of the scopes hierarchy
			auto prefix_list = mn::buf_with_allocator<const char*>(mn::memory::tmp());
			for (auto it = scope; it != nullptr; it = it->parent)
			{
				auto scope_name = mn::str_lit(it->name);
				if (scope_name.count == 0)
					continue;
				mn::buf_push(prefix_list, scope_name.ptr);
			}

			for (size_t i = 0; i < prefix_list.count; ++i)
			{
				auto prefix_name = prefix_list[prefix_list.count - i - 1];
				res = mn::strf(res, "{}_", prefix_name);
			}
			res = mn::strf(res, "{}", sym->name);
		}
		else
		{
			res = mn::strf(res, "{}", sym->name);
		}

		auto interned_res = unit_intern(self.unit->parent_unit, res.ptr);

		bool collided = false;
		// try to search the already generated names for this new name and if found
		// we'll try to make a new name for us
		for (auto it = scope; it != nullptr; it = it->parent)
		{
			if (auto name_it = mn::map_lookup(it->generated_names, interned_res))
			{
				res = mn::strf(res, "_{}", name_it->value + 1);
				auto interned_res = unit_intern(self.unit->parent_unit, res.ptr);
				++name_it->value;
				collided = true;
				break;
			}
		}

		if (collided == false)
		{
			mn::map_insert(scope->generated_names, interned_res, (size_t)1);
		}
		return interned_res;
	}

	inline static void
	_typer_resolve_symbol(Typer& self, Symbol* sym)
	{
		if (sym->state == STATE_RESOLVED)
		{
			return;
		}
		else if (sym->state == STATE_RESOLVING)
		{
			Err err{};
			err.loc = symbol_location(sym);
			err.msg = mn::strf("'{}' cyclic dependency", sym->name);
			unit_err(self.unit, err);
			return;
		}

		// TODO(Moustapha): maybe cache the typer instead of creating it every time
		auto old_typer = self;
		bool create_sub_typer = self.unit != sym->package;
		if (create_sub_typer)
			self = typer_new(sym->package);
		mn_defer({
			if (create_sub_typer)
			{
				typer_free(self);
				self = old_typer;
			}
		});

		sym->state = STATE_RESOLVING;
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
		case Symbol::KIND_ENUM:
			sym->type = type_interner_incomplete(self.unit->parent_unit->type_interner, sym);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
		sym->state = STATE_RESOLVED;

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
		{
			// don't resolve everything in the package just gather the top level symbols and
			// use it to lookup used symbols then only resolve the used symbols
			auto package = sym->package_sym.package;
			if (package->stage == COMPILATION_STAGE_CHECK)
			{
				auto sub_typer = typer_new(package);
				_typer_shallow_walk(sub_typer);
				typer_free(sub_typer);

				if (unit_package_has_errors(package))
					package->stage = COMPILATION_STAGE_FAILED;
				else
					package->stage = COMPILATION_STAGE_CODEGEN;
			}
			break;
		}
		case Symbol::KIND_STRUCT:
		case Symbol::KIND_ENUM:
			_typer_complete_type(self, sym, symbol_location(sym));
			break;
		default:
			assert(false && "unreachable");
			break;
		}

		// if sym is top level we add it to reachable symbols
		auto is_top_level = scope_is_top_level(self.global_scope, sym);
		if (auto decl = symbol_decl(sym))
		{
			is_top_level |= scope_is_top_level(decl->loc.file->file_scope, sym);
		}

		// we don't prepend scope for local variables
		bool prepend_scope = true;
		if (sym->kind == Symbol::KIND_VAR && is_top_level == false)
			prepend_scope = false;

		sym->package_name = _typer_generate_package_name_for_symbol(self, sym, prepend_scope);

		if (is_top_level ||
			sym->kind == Symbol::KIND_FUNC ||
			sym->kind == Symbol::KIND_FUNC_OVERLOAD_SET)
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

				if (cond_expr->const_value.type == type_bool && cond_expr->const_value.as_bool)
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

	inline static bool
	_typer_tag_table_has_semantic(const Tag_Table& tags)
	{
		// get the first tag without arguments
		for (const auto& [_, tag]: tags.table)
		{
			if (tag.args.count == 0)
				return true;
		}
		return false;
	}

	inline static void
	_typer_check_entry_struct_input(Typer& self, Type* type)
	{
		auto struct_decl = symbol_decl(type->struct_type.symbol);
		size_t struct_type_index = 0;
		for (const auto& field: struct_decl->struct_decl.fields)
		{
			const auto& struct_field = type->struct_type.fields[struct_type_index];

			if (type_is_shader_input(struct_field.type) == false)
			{
				Err err{};
				err.loc = struct_field.name.loc;
				err.msg = mn::strf("type '{}' cannot be used as shader input", struct_field.type);
				unit_err(self.unit, err);
			}
			struct_type_index += field.names.count;
		}
	}

	inline static void
	_typer_check_entry_input(Typer& self, Symbol* entry)
	{
		auto decl = symbol_decl(entry);
		auto type = entry->type;

		size_t type_index = 0;
		for (auto arg: decl->func_decl.args)
		{
			auto arg_type = type->as_func.sign.args.types[type_index];
			if (arg_type->kind == Type::KIND_STRUCT)
			{
				_typer_check_entry_struct_input(self, arg_type);
				type_index += arg.names.count;
				continue;
			}
			else if (arg_type->kind == Type::KIND_ARRAY)
			{
				auto base_type = arg_type->array.base;
				if (base_type->kind == Type::KIND_STRUCT)
				{
					_typer_check_entry_struct_input(self, base_type);
					type_index += arg.names.count;
					continue;
				}
			}

			Location err_loc{};
			if (arg.type.atoms.count > 0)
				err_loc = mn::buf_top(arg.type.atoms).named.type_name.loc;
			else if (arg.names.count > 0)
				err_loc = arg.names[0].loc;

			if (type_is_shader_input(arg_type) == false)
			{
				Err err{};
				err.loc = err_loc;
				err.msg = mn::strf("type '{}' cannot be used as shader input", arg_type);
				unit_err(self.unit, err);
			}
			type_index += arg.names.count;
		}

		// handle return type
		auto return_type = type->as_func.sign.return_type;
		// special case geometry shaders
		if (self.unit->parent_unit->mode == COMPILATION_MODE_GEOMETRY)
		{
			if (type_is_equal(return_type, type_void) == false)
			{
				Location err_loc{};
				if (decl->func_decl.return_type.atoms.count > 0)
					err_loc = mn::buf_top(decl->func_decl.return_type.atoms).named.type_name.loc;

				Err err{};
				err.loc = err_loc;
				err.msg = mn::strf("type '{}' cannot be used as shader output", return_type);
				unit_err(self.unit, err);
			}
		}
		else
		{
			if (return_type->kind == Type::KIND_STRUCT)
			{
				auto struct_decl = symbol_decl(return_type->struct_type.symbol);
				size_t struct_type_index = 0;
				for (const auto& field: struct_decl->struct_decl.fields)
				{
					const auto& struct_field = return_type->struct_type.fields[struct_type_index];

					if (mn::map_lookup(field.tags.table, KEYWORD_SV_POSITION) != nullptr)
					{
						if (struct_field.type != type_vec4)
						{
							Err err{};
							err.loc = struct_field.name.loc;
							err.msg = mn::strf("system position type is '{}', but it should be 'vec4'", struct_field.type);
							unit_err(self.unit, err);
						}
					}

					if (type_is_shader_input(struct_field.type) == false)
					{
						Err err{};
						err.loc = struct_field.name.loc;
						err.msg = mn::strf("type '{}' cannot be used as shader input", struct_field.type);
						unit_err(self.unit, err);
					}
					struct_type_index += field.names.count;
				}
			}
			else
			{
				Location err_loc{};
				if (decl->func_decl.return_type.atoms.count > 0)
					err_loc = mn::buf_top(decl->func_decl.return_type.atoms).named.type_name.loc;

				if (type_is_shader_input(return_type) == false)
				{
					Err err{};
					err.loc = err_loc;
					err.msg = mn::strf("type '{}' cannot be used as shader output", return_type);
					unit_err(self.unit, err);
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
		mn::buf_free(self.expected_expr_type);
	}

	void
	typer_check(Typer& self)
	{
		_typer_shallow_walk(self);

		auto compilation_unit = self.unit->parent_unit;
		Symbol* entry = nullptr;

		// check the entry function name if it does exist then we figure out
		// our compilation mode from the function tags
		if (compilation_unit->entry != nullptr)
		{
			entry = scope_find(self.unit->global_scope, compilation_unit->entry);
			if (entry == nullptr || entry->kind != Symbol::KIND_FUNC)
			{
				Location err_loc{};
				if (entry)
					if (auto decl = symbol_decl(entry))
						err_loc = decl->loc;

				Err err{};
				err.loc = err_loc;
				err.msg = mn::strf("entry point '{}' is not a function, or its name is not unique (it may be overloaded)", compilation_unit->entry);
				unit_err(self.unit, err);
			}
			else
			{
				auto decl = symbol_decl(entry);
				if (mn::map_lookup(decl->tags.table, KEYWORD_VERTEX) != nullptr)
				{
					compilation_unit->mode = COMPILATION_MODE_VERTEX;
					compilation_unit->entry_symbol = entry;
				}
				else if (mn::map_lookup(decl->tags.table, KEYWORD_PIXEL) != nullptr)
				{
					compilation_unit->mode = COMPILATION_MODE_PIXEL;
					compilation_unit->entry_symbol = entry;
				}
				else if (auto tag_it = mn::map_lookup(decl->tags.table, KEYWORD_GEOMETRY))
				{
					compilation_unit->mode = COMPILATION_MODE_GEOMETRY;
					compilation_unit->entry_symbol = entry;

					if (auto arg_it = mn::map_lookup(tag_it->value.args, KEYWORD_VERTEX_TYPE))
					{
						auto type_name = arg_it->value.value;
						compilation_unit->geometry_shader_output = _typer_find_symbol(self, type_name.str);
						if (compilation_unit->geometry_shader_output == nullptr)
						{
							Err err{};
							err.loc = type_name.loc;
							err.msg = mn::strf("cannot find geometry vertex output type '{}'", type_name.str);
							unit_err(self.unit, err);
						}
					}
				}
				else
				{
					Err err{};
					err.loc = decl->loc;
					err.msg = mn::strf("entry point is not tagged with @vertex or @pixel");
					unit_err(self.unit, err);
				}
			}
		}


		switch (compilation_unit->mode)
		{
		// library mode we check all the available global symbols
		case COMPILATION_MODE_LIBRARY:
			for (auto sym: self.global_scope->symbols)
				_typer_resolve_symbol(self, sym);
			break;
		// in case of vertex and pixel we start from the entry point
		case COMPILATION_MODE_VERTEX:
		case COMPILATION_MODE_PIXEL:
		case COMPILATION_MODE_GEOMETRY:
			_typer_resolve_symbol(self, entry);
			_typer_check_entry_input(self, entry);
			break;
		default:
			assert(false && "unreachable");
			break;
		}

		for (auto s: self.unit->parent_unit->reflected_symbols)
			_typer_resolve_symbol(self, s);
	}
}