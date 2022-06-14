#include "sabre/Check.h"
#include "sabre/Unit.h"
#include "sabre/Type_Interner.h"

#include <mn/IO.h>
#include <mn/Log.h>
#include <mn/Defer.h>
#include <mn/Assert.h>
#include <mn/Fixed_Buf.h>

namespace sabre
{
	const char SWIZZLE_XYZW[4] = {'x', 'y', 'z', 'w'};
	const char SWIZZLE_RGBA[4] = {'r', 'g', 'b', 'a'};

	inline static size_t
	_swizzle_style_index(const char* style, size_t size, mn::Rune r)
	{
		for (size_t i = 0; i < size; ++i)
			if (style[i] == r)
				return i;
		mn_unreachable();
		return SIZE_MAX;
	}

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

	inline static void
	_typer_enter_symbol(Typer& self, Symbol* symbol)
	{
		mn::buf_push(self.unit->parent_unit->symbol_stack, symbol);
	}

	inline static void
	_typer_leave_symbol(Typer& self)
	{
		mn_assert(self.unit->parent_unit->symbol_stack.count > 0);
		mn::buf_pop(self.unit->parent_unit->symbol_stack);
	}

	inline static void
	_typer_add_dependency(Typer& self, Symbol* symbol)
	{
		if (self.unit->parent_unit->symbol_stack.count > 0)
		{
			auto top = mn::buf_top(self.unit->parent_unit->symbol_stack);
			mn::set_insert(top->dependencies, symbol);
		}
	}

	inline static Scope*
	_typer_current_scope(const Typer& self)
	{
		return mn::buf_top(self.scope_stack);
	}

	inline static void
	_typer_enter_scope(Typer& self, Scope* scope)
	{
		mn_assert(scope != nullptr);
		mn::buf_push(self.scope_stack, scope);
	}

	inline static void
	_typer_leave_scope(Typer& self)
	{
		mn_assert(self.scope_stack.count > 1);
		mn::buf_pop(self.scope_stack);
	}

	inline static Decl*
	_typer_current_func(Typer& self)
	{
		if (self.func_stack.count > 0)
			return mn::buf_top(self.func_stack);
		return nullptr;
	}

	inline static void
	_typer_enter_func(Typer& self, Decl* decl)
	{
		mn::buf_push(self.func_stack, decl);
	}

	inline static void
	_typer_leave_func(Typer& self)
	{
		mn_assert(self.func_stack.count > 0);
		mn::buf_pop(self.func_stack);
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
		if (auto old_sym = scope_shallow_find(current_scope, sym->name); old_sym != nullptr && old_sym != sym)
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
		}

		// if we have different types then we can't assign
		return type_is_equal(lhs, rhs->type);
	}

	inline static void
	_typer_resolve_symbol(Typer& self, Symbol* sym);

	inline static Type*
	_typer_resolve_func_decl(Typer& self, Decl* d);

	inline static Type*
	_typer_resolve_expr(Typer& self, Expr* e);

	inline static void
	_typer_resolve_func_body_internal(Typer& self, Decl* d, Type* t, Scope* scope);

	inline static void
	_typer_shallow_walk(Typer& self);

	inline static void
	_typer_add_func_overload(Typer& self, Type* overload_set, Decl* decl)
	{
		decl->type = _typer_resolve_func_decl(self, decl);

		if (auto it = mn::map_lookup(overload_set->func_overload_set_type.overloads, decl->type->as_func.sign.args))
		{
			auto old_loc = it->value->loc;
			Err err{};
			err.loc = decl->loc;
			err.msg = mn::strf("function overload already defined {}:{}:{}", old_loc.file->filepath, old_loc.pos.line, old_loc.pos.col);
			unit_err(self.unit, err);
		}
		else
		{
			mn::map_insert(overload_set->func_overload_set_type.overloads, decl->type->as_func.sign.args, decl);
		}
	}

	inline static Symbol*
	_typer_add_func_symbol(Typer& self, Decl* decl)
	{
		mn_assert(decl->kind == Decl::KIND_FUNC);

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

		mn_assert(sym->kind == Symbol::KIND_FUNC_OVERLOAD_SET);
		// add the function declaration to overload set
		auto decl_type = type_void;
		if (sym->state == STATE_RESOLVED)
			decl_type = _typer_resolve_func_decl(self, decl);
		mn::map_insert(sym->func_overload_set_sym.decls, decl, decl_type);
		if (sym->state == STATE_RESOLVED)
		{
			mn_assert(sym->type->kind == Type::KIND_FUNC_OVERLOAD_SET);
			_typer_add_func_overload(self, sym->type, decl);
			auto scope = unit_create_scope_for(self.unit, decl, _typer_current_scope(self), decl->name.str, decl_type->as_func.sign.return_type, Scope::FLAG_NONE);
			_typer_resolve_func_body_internal(self, decl, decl_type, scope);
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
			mn_unreachable();
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
	_typer_template_instantiate(Typer& self, Type* base_type, const mn::Buf<Type*>& args, Location instantiation_loc, Decl* base_decl, bool suppress_errors)
	{
		if (base_type->template_args.count == 0)
		{
			if (suppress_errors == false)
			{
				Err err{};
				err.loc = instantiation_loc;
				err.msg = mn::strf(
					"type '{}' is not a template type",
					*base_type
				);
				unit_err(self.unit, err);
			}
			return base_type;
		}

		if (args.count != base_type->template_args.count)
		{
			if (suppress_errors == false)
			{
				Err err{};
				err.loc = instantiation_loc;
				err.msg = mn::strf(
					"template type expected #{} arguments, but #{} only was provided",
					base_type->template_args.count,
					args.count
				);
				unit_err(self.unit, err);
			}
			return base_type;
		}

		auto instantiated_types = mn::buf_with_allocator<Type*>(mn::memory::tmp());
		auto res = type_interner_template_instantiate(
			self.unit->parent_unit->type_interner,
			base_type,
			args,
			base_decl,
			&instantiated_types
		);

		for (auto t: instantiated_types)
		{
			if (type_is_templated(t))
				continue;

			if (type_is_struct(t))
			{
				auto instantiation_sym = symbol_struct_instantiation_new(self.unit->symbols_arena, t->struct_type.symbol, t);
				_typer_add_dependency(self, instantiation_sym);
				if (instantiation_sym->is_top_level)
					mn::buf_push(self.unit->reachable_symbols, instantiation_sym);
			}
		}

		return res;
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
				{
					res = named_type;

					// check if type is templated and missing its templates
					if (res == type_texture1d ||
						res == type_texture2d ||
						res == type_texture3d ||
						res == type_texture_cube)
					{
						auto args_types = mn::buf_with_allocator<Type*>(mn::memory::tmp());
						mn::buf_reserve(args_types, 1);
						mn::buf_push(args_types, type_vec4);
						res = _typer_template_instantiate(self, named_type, args_types, atom.named.type_name.loc, nullptr, false);
					}
					else if (type_is_templated(res))
					{
						auto sym = type_symbol(named_type);
						Decl* decl = nullptr;
						if (sym)
							decl = symbol_decl(sym);

						auto decl_args_indices = mn::buf_with_allocator<size_t>(mn::memory::tmp());
						if (decl && decl->template_args.count > 0)
						{
							for (size_t i = 0; i < decl->template_args.count; ++i)
								mn::buf_pushn(decl_args_indices, decl->template_args[i].names.count, i);
						}

						// we should do something with template arguments
						auto args_types = mn::buf_with_allocator<Type*>(mn::memory::tmp());
						mn::buf_reserve(args_types, named_type->template_args.count);
						for (size_t i = 0; i < named_type->template_args.count; ++i)
						{
							if (decl && i < decl_args_indices.count)
							{
								auto decl_index = decl_args_indices[i];
								auto default_type = decl->template_args[decl_index].default_type;

								// missing template argument with no default value, stop processing template arguments at this point
								if (default_type.atoms.count == 0)
									break;

								auto type = _typer_resolve_type_sign(self, default_type);
								mn::buf_push(args_types, type);
							}
						}
						res = _typer_template_instantiate(self, named_type, args_types, atom.named.type_name.loc, nullptr, false);
					}
				}
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
						err.msg = mn::strf("array count should be integer but found '{}'", *array_count_type);
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
				{
					auto sym = type_symbol(named_type);
					Decl* decl = nullptr;
					if (sym)
						decl = symbol_decl(sym);

					auto decl_args_indices = mn::buf_with_allocator<size_t>(mn::memory::tmp());
					if (decl && decl->template_args.count > 0)
					{
						for (size_t i = 0; i < decl->template_args.count; ++i)
							mn::buf_pushn(decl_args_indices, decl->template_args[i].names.count, i);
					}

					// we should do something with template arguments
					auto args_types = mn::buf_with_allocator<Type*>(mn::memory::tmp());
					mn::buf_reserve(args_types, named_type->template_args.count);
					for (size_t i = 0; i < named_type->template_args.count; ++i)
					{
						if (i < atom.templated.args.count)
						{
							auto type = _typer_resolve_type_sign(self, atom.templated.args[i]);
							mn::buf_push(args_types, type);
						}
						else if (decl && i < decl_args_indices.count)
						{
							auto decl_index = decl_args_indices[i];
							auto default_type = decl->template_args[decl_index].default_type;

							// missing template argument with no default value, stop processing template arguments at this point
							if (default_type.atoms.count == 0)
								break;

							auto type = _typer_resolve_type_sign(self, default_type);
							mn::buf_push(args_types, type);
						}
					}
					res = _typer_template_instantiate(self, named_type, args_types, atom.templated.type_name.loc, nullptr, false);
				}
				break;
			}
			default:
				mn_unreachable();
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
		case Tkn::KIND_LITERAL_STRING:
			e->mode = ADDRESS_MODE_CONST;
			e->const_value = expr_value_string(e->atom.tkn.str);
			return type_lit_string;
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
				e->symbol = sym;
				e->atom.decl = symbol_decl(sym);
				_typer_resolve_symbol(self, sym);
				if (sym->kind == Symbol::KIND_CONST)
				{
					if (sym->const_sym.value)
					{
						e->const_value = sym->const_sym.value->const_value;
					}
					else if (mn::map_lookup(sym->const_sym.decl->tags.table, KEYWORD_BUILD_BACKEND) != nullptr)
					{
						e->const_value = expr_value_int(self.unit->parent_unit->backend);
					}
				}
				else if (sym->kind == Symbol::KIND_PACKAGE)
				{
					e->const_value = expr_value_package(sym);
				}

				if (sym->kind == Symbol::KIND_CONST || sym->kind == Symbol::KIND_PACKAGE)
				{
					e->mode = ADDRESS_MODE_CONST;
				}
				else if (sym->kind == Symbol::KIND_VAR)
				{
					if (sym->var_sym.is_buffer && sym->var_sym.is_read_write == false)
					{
						e->mode = ADDRESS_MODE_READ_ONLY;
					}
					else if (sym->var_sym.is_uniform)
					{
						e->mode = ADDRESS_MODE_READ_ONLY;
					}
					else
					{
						e->mode = ADDRESS_MODE_VARIABLE;
					}
				}
				else if (sym->kind == Symbol::KIND_FUNC && sym->type->as_func.sign.return_type != type_void)
				{
					e->mode = ADDRESS_MODE_COMPUTED_VALUE;
				}

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
			mn_unreachable();
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
					err.msg = mn::strf("width mismatch in multiply operation '{}' * '{}'", *lhs_type, *rhs_type);
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
					err.msg = mn::strf("width mismatch in multiply operation '{}' * '{}'", *lhs_type, *rhs_type);
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
					err.msg = mn::strf("illegal binary operation on vector type, lhs is '{}' and rhs is '{}'", *lhs_type, *rhs_type);
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
					err.msg = mn::strf("illegal binary operation on vector type, lhs is '{}' and rhs is '{}'", *lhs_type, *rhs_type);
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
				err.msg = mn::strf("type '{}' doesn't support bitwise operations", *lhs_type);
				unit_err(self.unit, err);
			}

			if (type_has_bit_ops(rhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.right->loc;
				err.msg = mn::strf("type '{}' doesn't support bitwise operations", *rhs_type);
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
				err.msg = mn::strf("type '{}' doesn't support bitwise operations", *lhs_type);
				unit_err(self.unit, err);
			}

			if (type_has_bit_ops(rhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.right->loc;
				err.msg = mn::strf("type '{}' doesn't support bitwise operations", *rhs_type);
				unit_err(self.unit, err);
			}
		}
		else if (e->binary.op.kind == Tkn::KIND_PLUS ||
				 e->binary.op.kind == Tkn::KIND_MINUS ||
				 e->binary.op.kind == Tkn::KIND_STAR ||
				 e->binary.op.kind == Tkn::KIND_DIVIDE ||
				 e->binary.op.kind == Tkn::KIND_MODULUS)
		{
			if (type_has_arithmetic(lhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.left->loc;
				err.msg = mn::strf("type '{}' doesn't support arithmetic operations", *lhs_type);
				unit_err(self.unit, err);
			}

			if (type_has_arithmetic(rhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.right->loc;
				err.msg = mn::strf("type '{}' doesn't support arithmetic operations", *rhs_type);
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
					err.msg = mn::strf("type '{}' cannot be used in a bitwise shift operation", *rhs_type);
					unit_err(self.unit, err);
				}
				else if (type_width(lhs_type) != type_width(rhs_type))
				{
					Err err{};
					err.loc = e->binary.right->loc;
					err.msg = mn::strf("type '{}' is not compatible with '{}' in a bitwise shift operation", *lhs_type, *rhs_type);
					unit_err(self.unit, err);
				}
			}
			else if (type_is_equal(lhs_type, type_lit_int) && type_is_equal(rhs_type, type_uint))
			{
				if (e->binary.left->mode == ADDRESS_MODE_CONST && e->binary.left->const_value.as_int >= 0)
				{
					// we permit this because it's a positive constant literal
				}
				else
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("type mismatch in binary expression, lhs is '{}' and rhs is '{}'", *lhs_type, *rhs_type);
					unit_err(self.unit, err);
					failed = true;
				}
			}
			else if (type_is_equal(lhs_type, type_uint), type_is_equal(rhs_type, type_lit_int))
			{
				if (e->binary.right->mode == ADDRESS_MODE_CONST && e->binary.right->const_value.as_int >= 0)
				{
					// we permit this because it's a positive constant literal
				}
				else
				{
					Err err{};
					err.loc = e->loc;
					err.msg = mn::strf("type mismatch in binary expression, lhs is '{}' and rhs is '{}'", *lhs_type, *rhs_type);
					unit_err(self.unit, err);
					failed = true;
				}
			}
			else
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("type mismatch in binary expression, lhs is '{}' and rhs is '{}'", *lhs_type, *rhs_type);
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
				err.msg = mn::strf("logical operators only work on boolean types, but found '{}'", *lhs_type);
				unit_err(self.unit, err);
			}

			if (type_is_bool_like(rhs_type) == false)
			{
				Err err{};
				err.loc = e->binary.right->loc;
				err.msg = mn::strf("logical operators only work on boolean types, but found '{}'", *rhs_type);
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
				err.msg = mn::strf("'{}' is only allowed for numeric types, but expression type is '{}'", e->unary.op.str, *type);
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
				err.msg = mn::strf("'{}' is only allowed for numeric types, but expression type is '{}'", e->unary.op.str, *type);
				unit_err(self.unit, err);
			}
		}
		else if (e->unary.op.kind == Tkn::KIND_LOGICAL_NOT)
		{
			if (type_is_equal(type, type_bool) == false)
			{
				Err err{};
				err.loc = e->unary.base->loc;
				err.msg = mn::strf("logical not operator is only allowed for boolean types, but expression type is '{}'", e->unary.op.str, *type);
				unit_err(self.unit, err);
			}
		}
		else if (e->unary.op.kind == Tkn::KIND_BIT_NOT)
		{
			if (type_has_bit_ops(type) == false)
			{
				Err err{};
				err.loc = e->unary.base->loc;
				err.msg = mn::strf("type '{}' cannot be used in a bit not operation", *type);
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

	struct Resolved_Type
	{
		Type* type;
		bool ok;
	};

	inline static Resolved_Type
	_typer_resolve_expected_type_from_arg_type(Typer& self, Type* expected_type, Type* arg_type, Location arg_loc, mn::Map<Type*, Type*>& resolved_types, int depth, bool has_explicit_template_args)
	{
		if (type_is_typename(expected_type))
		{
			if (auto it = mn::map_lookup(resolved_types, expected_type))
			{
				if (type_is_equal(it->value, arg_type) == false)
				{
					if (has_explicit_template_args == false || (has_explicit_template_args && depth == 0))
					{
						Err err{};
						err.loc = arg_loc;
						err.msg = mn::strf("type '{}' is ambiguous, we already deduced it to be '{}' but we have another guess which is '{}'", *expected_type, *it->value, *arg_type);
						unit_err(self.unit, err);
					}

					Resolved_Type res{};
					res.type = it->value;
					res.ok = false;
					return res;
				}
				else
				{
					Resolved_Type res{};
					res.type = arg_type;
					res.ok = true;
					return res;
				}
			}
			else
			{
				mn::map_insert(resolved_types, expected_type, arg_type);
				Resolved_Type res{};
				res.type = arg_type;
				res.ok = true;
				return res;
			}
		}
		else if (type_is_templated(expected_type))
		{
			auto min_args = expected_type->full_template_args.count;
			if (min_args > arg_type->template_base_args.count)
				min_args = arg_type->template_base_args.count;

			auto ok = expected_type->full_template_args.count == arg_type->template_base_args.count;
			auto instantiation_args = mn::buf_with_allocator<Type*>(mn::memory::tmp());
			for (size_t i = 0; i < min_args; ++i)
			{
				auto single_res = _typer_resolve_expected_type_from_arg_type(self, expected_type->full_template_args[i], arg_type->template_base_args[i], arg_loc, resolved_types, depth + 1, has_explicit_template_args);
				mn::buf_push(instantiation_args, single_res.type);
				ok &= single_res.ok;
			}
			Resolved_Type res{};
			res.type = _typer_template_instantiate(self, expected_type, instantiation_args, arg_loc, nullptr, true);
			res.ok = ok;
			if (has_explicit_template_args && res.ok == false && depth == 0)
			{
				Err err{};
				err.loc = arg_loc;
				err.msg = mn::strf("argument type mismatch, expected '{}' but found '{}'", *res.type, *arg_type);
				unit_err(self.unit, err);
			}
			return res;
		}
		else
		{
			Resolved_Type res{};
			res.type = arg_type;
			res.ok = type_is_equal(expected_type, arg_type);
			return res;
		}
	}

	inline static bool
	_typer_guess_template_func_call_types(Typer& self, Type* func_type, const mn::Buf<Expr*>& args, mn::Map<Type*, Type*>& resolved_types)
	{
		bool has_explicit_template_args = resolved_types.count > 0;
		bool res = true;
		for (size_t i = 0; i < args.count; ++i)
		{
			auto arg = args[i];
			auto arg_type = _typer_resolve_expr(self, arg);
			auto expected_type = func_type->as_func.sign.args.types[i];
			auto single_res = _typer_resolve_expected_type_from_arg_type(self, expected_type, arg_type, arg->loc, resolved_types, 0, has_explicit_template_args);
			res &= single_res.ok;
		}
		return res;
	}

	inline static int
	_typer_type_similarity_score(Type* a, Type* b)
	{
		if (type_is_equal(a, b))
			return 1;

		if (type_is_typename(a) || type_is_typename(b))
			return 0;

		int score = 0;
		for (auto it = a->template_base_type; it != nullptr; it = it->template_base_type)
		{
			for (auto it2 = b; it2 != nullptr; it2 = it2->template_base_type)
			{
				score += _typer_type_similarity_score(it, it2);
			}
		}
		return score;
	}

	struct Overload_Candidate
	{
		Decl* original_decl;
		Decl* instantiated_decl;
		int score;
	};

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
			if (symbol)
				e->call.func = symbol->func_sym.decl;

			if (e->call.args.count != type->as_func.sign.args.types.count)
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("function expected {} arguments, but {} were provided", type->as_func.sign.args.types.count, e->call.args.count);
				unit_err(self.unit, err);
				return type->as_func.sign.return_type;
			}

			auto resolved_types = mn::map_with_allocator<Type*, Type*>(mn::memory::tmp());
			if (type_is_templated(type))
			{
				for (size_t i = 0; i < e->call.template_args.count; ++i)
				{
					auto explicit_type = _typer_resolve_type_sign(self, e->call.template_args[i]);
					mn::map_insert(resolved_types, type->template_args[i], explicit_type);
				}

				auto is_guess_ok = _typer_guess_template_func_call_types(self, _typer_resolve_expr(self, e->call.base), e->call.args, resolved_types);
				if (is_guess_ok)
				{
					auto arg_types = mn::buf_with_allocator<Type*>(mn::memory::tmp());

					for (size_t i = 0; i < type->template_args.count; ++i)
					{
						auto template_arg = type->template_args[i];
						if (auto it = mn::map_lookup(resolved_types, template_arg))
						{
							mn::buf_push(arg_types, it->value);
						}
						else
						{
							auto decl = symbol->func_sym.decl;
							if (auto default_type = _typer_resolve_type_sign(self, decl->template_args[i].default_type))
							{
								mn::buf_push(arg_types, default_type);
							}
							else
							{
								mn::buf_push(arg_types, type_void);
							}
						}
					}

					auto instantiated_type = _typer_template_instantiate(self, type, arg_types, e->loc, e->call.func, false);
					if (auto decl = type_interner_find_func_instantiation_decl(self.unit->parent_unit->type_interner, type, arg_types))
					{
						// do nothing we have already instantiated this function
					}
					else
					{
						auto templated_decl = symbol->func_sym.decl;
						auto instantiated_decl = decl_clone(templated_decl, templated_decl->arena);
						instantiated_decl->type = instantiated_type;
						type_interner_add_func_instantiation_decl(self.unit->parent_unit->type_interner, type, arg_types, instantiated_decl);

						auto instantiation_sym = symbol_func_instantiation_new(self.unit->symbols_arena, symbol, instantiated_type, instantiated_decl);
						_typer_add_dependency(self, instantiation_sym);
						mn::buf_push(self.unit->reachable_symbols, instantiation_sym);

						e->call.func = instantiated_decl;
						e->call.base->symbol = instantiation_sym;
						auto templated_scope = unit_scope_find(self.unit->parent_unit, templated_decl);
						auto instantiated_scope = unit_create_scope_for(self.unit, instantiated_decl, templated_scope->parent, instantiated_decl->name.str, instantiated_type->as_func.sign.return_type, Scope::FLAG_NONE);
						_typer_enter_scope(self, instantiated_scope);
						{
							// push symbols for typenames but after actually resolving them
							size_t i = 0;
							for (auto template_arg: instantiated_decl->template_args)
							{
								for (auto name: template_arg.names)
								{
									auto v = symbol_typename_new(self.unit->symbols_arena, name);
									v->type = arg_types[i];
									_typer_add_symbol(self, v);
									++i;
								}
							}

							// push arguments to instantiated scope
							i = 0;
							for (auto arg: instantiated_decl->func_decl.args)
							{
								auto arg_type = instantiated_type->as_func.sign.args.types[i];
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

						auto err_count = self.unit->errs.count;
						_typer_resolve_func_body_internal(self, instantiated_decl, instantiated_type, instantiated_scope);
						if (self.unit->errs.count > err_count)
						{
							Err err{};
							err.is_note = true;
							err.loc = e->loc;
							err.msg = mn::strf("call to template function '{}' has errors, it was instantiated with the following template arguments:\n", templated_decl->name.str);
							for (size_t i = 0; i < instantiated_type->template_base_args.count; ++i)
							{
								if (i > 0)
									err.msg = mn::strf(err.msg, "\n");
								err.msg = mn::strf(err.msg, "  - {} = {}", *instantiated_type->template_base_type->template_args[i], *instantiated_type->template_base_args[i]);
							}
							unit_err(self.unit, err);
						}
					}
					type = instantiated_type;
				}
			}

			for (size_t i = 0; i < e->call.args.count; ++i)
			{
				auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
				auto func_arg_type = type->as_func.sign.args.types[i];
				if (_typer_can_assign(func_arg_type, e->call.args[i]) == false)
				{
					if (type_is_templated(func_arg_type) || type_is_typename(func_arg_type))
					{
						if (auto it = mn::map_lookup(resolved_types, func_arg_type))
						{
							if (_typer_can_assign(it->value, e->call.args[i]) == false)
							{
								Err err{};
								err.loc = e->call.args[i]->loc;
								err.msg = mn::strf("function argument #{} type mismatch, expected '{}' but found '{}'", i, *it->value, *arg_type);
								unit_err(self.unit, err);
							}
						}
					}
					else
					{
						Err err{};
						err.loc = e->call.args[i]->loc;
						err.msg = mn::strf("function argument #{} type mismatch, expected '{}' but found '{}'", i, *func_arg_type, *arg_type);
						unit_err(self.unit, err);
					}
				}
			}

			return type->as_func.sign.return_type;
		}
		else if (type->kind == Type::KIND_FUNC_OVERLOAD_SET)
		{
			auto overload_set_symbol = type->func_overload_set_type.symbol;
			auto templated_candidates = mn::buf_with_allocator<Decl*>(mn::memory::tmp());
			Decl* exact_decl = nullptr;
			for (auto& [overload_decl, overload_type]: overload_set_symbol->func_overload_set_sym.decls)
			{
				if (e->call.args.count != overload_type->as_func.sign.args.types.count)
					continue;

				if (type_is_templated(overload_type))
				{
					mn::buf_push(templated_candidates, overload_decl);
					continue;
				}

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
					exact_decl = overload_decl;
					if (e->call.base->kind == Expr::KIND_ATOM)
						e->call.base->atom.decl = exact_decl;
					e->call.func = exact_decl;
					if (mn::set_lookup(overload_set_symbol->func_overload_set_sym.unique_used_decls, overload_decl) == nullptr)
					{
						mn::buf_push(overload_set_symbol->func_overload_set_sym.used_decls, overload_decl);
						mn::set_insert(overload_set_symbol->func_overload_set_sym.unique_used_decls, overload_decl);
					}
					break;
				}
			}

			if (exact_decl == nullptr && templated_candidates.count > 0)
			{
				auto overload_candidates = mn::buf_with_allocator<Overload_Candidate>(mn::memory::tmp());
				for (auto candidate: templated_candidates)
				{
					auto resolved_types = mn::map_with_allocator<Type*, Type*>(mn::memory::tmp());
					for (size_t i = 0; i < e->call.template_args.count; ++i)
					{
						auto explicit_type = _typer_resolve_type_sign(self, e->call.template_args[i]);
						mn::map_insert(resolved_types, candidate->type->template_args[i], explicit_type);
					}
					auto is_guess_ok = _typer_guess_template_func_call_types(self, candidate->type, e->call.args, resolved_types);
					if (is_guess_ok == false)
						continue;

					auto arg_types = mn::buf_with_allocator<Type*>(mn::memory::tmp());
					for (size_t i = 0; i < candidate->type->template_args.count; ++i)
					{
						auto template_arg = candidate->type->template_args[i];
						if (auto it = mn::map_lookup(resolved_types, template_arg))
						{
							mn::buf_push(arg_types, it->value);
						}
						else
						{
							if (auto default_type = _typer_resolve_type_sign(self, candidate->template_args[i].default_type))
							{
								mn::buf_push(arg_types, default_type);
							}
							else
							{
								mn::buf_push(arg_types, type_void);
							}
						}
					}

					auto instantiated_type = _typer_template_instantiate(self, candidate->type, arg_types, e->loc, candidate, false);
					Decl* instantiated_decl = nullptr;
					if (auto decl = type_interner_find_func_instantiation_decl(self.unit->parent_unit->type_interner, candidate->type, arg_types))
					{
						instantiated_decl = decl;
					}
					else
					{
						auto templated_decl = candidate;
						instantiated_decl = decl_clone(templated_decl, templated_decl->arena);
						instantiated_decl->type = instantiated_type;
						type_interner_add_func_instantiation_decl(self.unit->parent_unit->type_interner, candidate->type, arg_types, instantiated_decl);

						auto templated_scope = unit_scope_find(self.unit->parent_unit, templated_decl);
						auto instantiated_scope = unit_create_scope_for(self.unit, instantiated_decl, templated_scope->parent, instantiated_decl->name.str, instantiated_type->as_func.sign.return_type, Scope::FLAG_NONE);
						_typer_enter_scope(self, instantiated_scope);
						{
							// push symbols for typenames but after actually resolving them
							size_t i = 0;
							for (auto template_arg: instantiated_decl->template_args)
							{
								for (auto name: template_arg.names)
								{
									auto v = symbol_typename_new(self.unit->symbols_arena, name);
									v->type = arg_types[i];
									_typer_add_symbol(self, v);
									++i;
								}
							}

							// push arguments to instantiated scope
							i = 0;
							for (auto arg: instantiated_decl->func_decl.args)
							{
								auto arg_type = instantiated_type->as_func.sign.args.types[i];
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

						auto err_count = self.unit->errs.count;
						_typer_resolve_func_body_internal(self, instantiated_decl, instantiated_type, instantiated_scope);
						if (self.unit->errs.count > err_count)
						{
							// we ignore this candidate since it has errors but we'll not actually issue any errors
							// because other candidates might not have errors
							for (size_t i = err_count; i < self.unit->errs.count; ++i)
							{
								err_free(self.unit->errs[i]);
							}
							mn::buf_resize(self.unit->errs, err_count);
							instantiated_decl = nullptr;
						}
					}

					// mn::log_debug("call at line: {}", e->loc.pos.line);
					int score = 0;
					for (size_t i = 0; i < e->call.args.count; ++i)
					{
						auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
						auto template_type = candidate->type->as_func.sign.args.types[i];
						score += _typer_type_similarity_score(arg_type, template_type);
						// mn::log_debug("{} vs {} = {}", *arg_type, *template_type, _typer_type_similarity_score(arg_type, template_type));
					}
					// mn::log_debug("candidate {}, score {}", *candidate->type, score);
					mn::buf_push(overload_candidates, Overload_Candidate{candidate, instantiated_decl, score});
				}

				std::stable_sort(begin(overload_candidates), end(overload_candidates), [](const auto& a, const auto& b){
					return a.score > b.score;
				});

				if (overload_candidates.count > 0)
				{
					auto best_match_score = overload_candidates[0].score;
					size_t same_score_count = 1;
					for (size_t i = 1; i < overload_candidates.count; ++i)
						if (overload_candidates[i].score == best_match_score)
							++same_score_count;

					if (same_score_count > 1)
					{
						auto msg = mn::strf("ambiguous function call 'func(");

						for (size_t i = 0; i < e->call.args.count; ++i)
						{
							if (i > 0)
								msg = mn::strf(msg, ", ");

							auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
							msg = mn::strf(msg, ":{}", *arg_type);
						}
						msg = mn::strf(msg, ")' in the overload set:");

						for (size_t i = 0; i < same_score_count; ++i)
						{
							auto candidate = overload_candidates[i].original_decl;
							msg = mn::strf(
								msg,
								"\n  {}. {} defined in {}:{}:{}",
								i,
								*candidate->type,
								candidate->loc.file->filepath,
								candidate->loc.pos.line,
								candidate->loc.pos.col
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
						exact_decl = overload_candidates[0].instantiated_decl;
						auto symbol = e->call.base->symbol;
						symbol_func_instantiation_new(self.unit->symbols_arena, symbol, exact_decl->type, exact_decl);
						_typer_add_dependency(self, exact_decl->symbol);
						mn::buf_push(self.unit->reachable_symbols, exact_decl->symbol);

						e->call.func = exact_decl;
						e->call.base->symbol = exact_decl->symbol;
					}
				}
			}

			if (exact_decl == nullptr)
			{
				auto msg = mn::strf("cannot find suitable function for 'func(");

				for (size_t i = 0; i < e->call.args.count; ++i)
				{
					if (i > 0)
						msg = mn::strf(msg, ", ");

					auto arg_type = _typer_resolve_expr(self, e->call.args[i]);
					msg = mn::strf(msg, ":{}", *arg_type);
				}
				msg = mn::strf(msg, ")' in the overload set:");

				auto overload_i = 0;
				for (auto [_, overload]: type->func_overload_set_type.overloads)
				{
					msg = mn::strf(
						msg,
						"\n  {}. {} defined in {}:{}:{}",
						overload_i++,
						*overload->type,
						overload->loc.file->filepath,
						overload->loc.pos.line,
						overload->loc.pos.col
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
				return exact_decl->type->as_func.sign.return_type;
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
			err.msg = mn::strf("cannot cast '{}' to '{}'", *from_type, *to_type);
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

		if (type == nullptr)
		{
			if (e->dot.rhs &&
				e->dot.rhs->kind == Expr::KIND_ATOM &&
				(e->dot.rhs->atom.tkn.kind == Tkn::KIND_LITERAL_INTEGER ||
				 e->dot.rhs->atom.tkn.kind == Tkn::KIND_LITERAL_FLOAT))
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("Did you mean 0.{}?, you cannot omit 0 in floating point numbers", e->dot.rhs->atom.tkn.str);
				unit_err(self.unit, err);
				return type_void;
			}
			else
			{
				Err err{};
				err.loc = e->loc;
				err.msg = mn::strf("we couldn't deduce lhs type of a dot expression from context, please provide it explicity");
				unit_err(self.unit, err);
				return type_void;
			}
		}

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

			auto res = type_vectorize(type->vec.base, len);
			e->mode = e->dot.lhs->mode;

			if (e->mode == ADDRESS_MODE_CONST)
			{
				e->const_value = expr_value_aggregate(e->loc.file->ast_arena, res);
				for (auto r: mn::str_runes(e->dot.rhs->atom.tkn.str))
				{
					auto index = _swizzle_style_index(swizzle_style, 4, r);
					auto src_value = expr_value_aggregate_get(e->dot.lhs->const_value, index);
					expr_value_aggregate_set(e->const_value, index, src_value);
				}
			}

			if (len == 1)
			{
				auto r = mn::rune_read(e->dot.rhs->atom.tkn.str);
				e->dot.unaligned_offset = _swizzle_style_index(swizzle_style, type->vec.width, r) * type->vec.base->unaligned_size;
				e->dot.aligned_offset = e->dot.unaligned_offset;
				e->dot.has_offset = true;
			}
			else
			{
				e->dot.unaligned_offset = 0;
				e->dot.aligned_offset = e->dot.unaligned_offset;
				e->dot.has_offset = true;
			}
			return res;
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
			if (e->mode == ADDRESS_MODE_CONST)
				e->const_value = expr_value_aggregate_get(e->dot.lhs->const_value, it->value);
			e->symbol = type->struct_type.symbol;
			e->dot.unaligned_offset = type->struct_type.fields[it->value].unaligned_offset;
			e->dot.aligned_offset = type->struct_type.fields[it->value].aligned_offset;
			e->dot.has_offset = true;
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

			e->dot.rhs->symbol = symbol;
			e->dot.rhs->atom.decl = symbol_decl(symbol);
			_typer_resolve_symbol(self, symbol);
			e->symbol = symbol;
			if (symbol->kind == Symbol::KIND_CONST && symbol->const_sym.value != nullptr)
			{
				e->const_value = symbol->const_sym.value->const_value;
				e->mode = symbol->const_sym.value->mode;
			}
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
		if (type_is_indexable(base_type) == false)
		{
			Err err{};
			err.loc = e->loc;
			err.msg = mn::strf("type '{}' is not indexable", *base_type);
			unit_err(self.unit, err);
			return base_type;
		}

		auto allowed_index_types = mn::fixed_buf_new<Type*, 2>();

		auto res = type_void;
		size_t count = 0;
		bool is_bounded = false;
		if (type_is_array(base_type))
		{
			res = base_type->array.base;
			count = base_type->array.count;
			is_bounded = type_is_bounded_array(base_type);
			mn::fixed_buf_push(allowed_index_types, type_int);
			mn::fixed_buf_push(allowed_index_types, type_uint);
		}
		else if (type_is_matrix(base_type))
		{
			res = type_vectorize(base_type->mat.base, base_type->mat.width);
			count = base_type->mat.width;
			is_bounded = true;
			mn::fixed_buf_push(allowed_index_types, type_int);
			mn::fixed_buf_push(allowed_index_types, type_uint);
		}
		else if (base_type->kind == Type::KIND_RW_TEXTURE)
		{
			res = base_type->full_template_args[0];
			if (base_type->texture.type == TEXTURE_TYPE_1D)
			{
				mn::fixed_buf_push(allowed_index_types, type_int);
				mn::fixed_buf_push(allowed_index_types, type_uint);
			}
			else if (base_type->texture.type == TEXTURE_TYPE_2D)
			{
				mn::fixed_buf_push(allowed_index_types, type_ivec2);
				mn::fixed_buf_push(allowed_index_types, type_uvec2);
			}
			else if (base_type->texture.type == TEXTURE_TYPE_3D)
			{
				mn::fixed_buf_push(allowed_index_types, type_ivec3);
				mn::fixed_buf_push(allowed_index_types, type_uvec3);
			}
			else if (base_type->texture.type == TEXTURE_TYPE_CUBE)
			{
				mn::fixed_buf_push(allowed_index_types, type_ivec3);
				mn::fixed_buf_push(allowed_index_types, type_uvec3);
			}
			else
			{
				mn_unreachable();
			}
		}
		else
		{
			mn_unreachable();
		}

		auto index_type = _typer_resolve_expr(self, e->indexed.index);
		bool index_is_allowed = false;
		for (auto allowed_index_type: allowed_index_types)
		{
			if (type_is_equal(allowed_index_type, index_type))
			{
				index_is_allowed = true;
				break;
			}
		}

		if (index_is_allowed == false)
		{
			auto msg = mn::strf("index type should be ");
			for (size_t i = 0; i < allowed_index_types.count; ++i)
			{
				if (i > 0)
					msg = mn::strf(msg, ", or ");
				msg = mn::strf(msg, "'{}'", *allowed_index_types[i]);
			}
			msg = mn::strf(msg, ", but we found '{}'", *index_type);

			Err err{};
			err.loc = e->indexed.index->loc;
			err.msg = msg;
			unit_err(self.unit, err);
			return res;
		}

		if (e->indexed.index->mode == ADDRESS_MODE_CONST &&
			e->indexed.index->const_value.type == type_int &&
			e->indexed.index->const_value.as_int >= count &&
			is_bounded)
		{
			Err err{};
			err.loc = e->indexed.index->loc;
			err.msg = mn::strf(
				"array index out of range, array count is '{}' but index is '{}'",
				count,
				e->indexed.index->const_value.as_int
			);
			unit_err(self.unit, err);
		}

		// arrays have variable mode by default, unless they are constants
		e->mode = e->indexed.base->mode;
		if (e->indexed.base->mode == ADDRESS_MODE_CONST &&
			e->indexed.index->mode == ADDRESS_MODE_CONST)
		{
			if ((e->indexed.base->const_value.type && type_is_array(e->indexed.base->const_value.type)) &&
				e->indexed.index->const_value.type == type_int)
			{
				if (e->indexed.index->const_value.as_int < count)
				{
					e->mode = ADDRESS_MODE_CONST;
					e->const_value = expr_value_aggregate_get(e->indexed.base->const_value, e->indexed.index->const_value.as_int);
				}
			}
		}

		return res;
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
						err.msg = mn::strf("type '{}' doesn't have field '{}'", *type_it, field.selector_name->atom.tkn.str);
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
						err.msg = mn::strf("type '{}' doesn't have field '{}'", *type_it, field.selector_name->atom.tkn.str);
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
					err.msg = mn::strf("type '{}' doesn't have field '{}'", *type_it, field.selector_name->atom.tkn.str);
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
						err.msg = mn::strf("type '{}' contains only {} fields", *type_it, type_it->vec.width);
						unit_err(self.unit, err);
						failed = true;
					}
				}
				else if (type_it->kind == Type::KIND_MAT)
				{
					if (type_field_index < type_it->mat.width)
					{
						type_it = type_vectorize(type_it->mat.base, type_it->mat.width);
						field.selector_index = type_field_index;
						++type_field_index;
					}
					else
					{
						Err err{};
						err.loc = field.value->loc;
						err.msg = mn::strf("type '{}' contains only {} fields", *type_it, type_it->vec.width);
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
						err.msg = mn::strf("type '{}' contains only {} fields", *type_it, type_it->struct_type.fields.count);
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
						err.msg = mn::strf("array '{}' contains only {} elements", *type_it, type_it->array.count);
						unit_err(self.unit, err);
						failed = true;
					}
				}
				else
				{
					Err err{};
					err.loc = field.value->loc;
					err.msg = mn::strf("type '{}' doesn't have fields", *type_it);
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
						err.msg = mn::strf("type mismatch in compound literal value, type '{}' cannot be constructed from '{}'", *type, *value_type);
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
					err.msg = mn::strf("type mismatch in compound literal value, selector type '{}' but expression type is '{}'", *type_it, *value_type);
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
				// mn_unreachable_msg("only constant arrays are handled now");
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
			mn_unreachable();
			e->type = type_void;
			break;
		}

		return e->type;
	}

	inline static void
	_typer_resolve_tags(Typer& self, Tag_Table& tags)
	{
		for (auto& [_, tag]: tags.table)
		{
			for (auto& [_, kv]: tag.args)
			{
				auto type = _typer_resolve_expr(self, kv.value);
				auto mode = kv.value->mode;
				if (mode != ADDRESS_MODE_CONST)
				{
					Err err{};
					err.loc = kv.value->loc;
					err.msg = mn::strf("tags can only accept constant expressions, the provided expression is not a constant");
					unit_err(self.unit, err);
					continue;
				}

				auto expr_value = kv.value->const_value;
				if (expr_value.type != type_int &&
					expr_value.type != type_lit_string)
				{
					Err err{};
					err.loc = kv.value->loc;
					err.msg = mn::strf("tag value expressions can only be of integer or string types");
					unit_err(self.unit, err);
				}
			}
		}
	}

	inline static Type*
	_typer_resolve_const(Typer& self, Symbol* sym)
	{
		_typer_resolve_tags(self, sym->const_sym.decl->tags);

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
					err.msg = mn::strf("type mismatch expected '{}' but found '{}'", *res, *expr_type);
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
		else if (type->kind == Type::KIND_RW_TEXTURE)
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
					err.msg = mn::strf("field type '{}' cannot be used for uniform", *field.type);
					unit_err(self.unit, err);
				}
			}
			return res;
		}
		else if (type_is_unbounded_array(type))
		{
			Err err{};
			err.msg = mn::strf("'{}' unbounded arrays cannot be used in uniforms", *type);
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

	inline static bool
	_typer_check_type_suitable_for_buffer(Typer& self, Type* type, size_t depth, bool last_field)
	{
		if (type_is_struct(type))
		{
			bool res = true;
			for (size_t i = 0; i < type->struct_type.fields.count; ++i)
			{
				auto field = type->struct_type.fields[i];
				bool field_res = _typer_check_type_suitable_for_buffer(self, field.type, depth + 1, i + 1 == type->struct_type.fields.count);
				res &= field_res;

				if (field_res == false)
				{
					Err err{};
					err.loc = field.name.loc;
					err.msg = mn::strf("field type '{}' cannot be used for buffer", *field.type);
					unit_err(self.unit, err);
				}
			}
			return res;
		}
		else if (type_is_unbounded_array(type))
		{
			if (depth > 1 || last_field == false)
			{
				Err err{};
				err.msg = mn::strf("'{}' unbounded arrays cannot be used in buffers", *type);
				unit_err(self.unit, err);
				return false;
			}
			else
			{
				return _typer_check_type_suitable_for_buffer(self, type->array.base, depth + 1, last_field);
			}
		}
		else if (type_is_bounded_array(type))
		{
			return _typer_check_type_suitable_for_buffer(self, type->array.base, depth + 1, last_field);
		}
		else
		{
			return type_is_uniform(type);
		}
	}

	inline static Type*
	_typer_resolve_var(Typer& self, Symbol* sym)
	{
		_typer_resolve_tags(self, sym->var_sym.decl->tags);
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
					err.msg = mn::strf("type mismatch expected '{}' but found '{}'", *res, *expr_type);
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
				err.msg = mn::strf("uniform variable type '{}' contains types which cannot be used in a uniform", *res);
				unit_err(self.unit, err);
			}
			else
			{
				sym->var_sym.is_uniform = true;
				mn::buf_push(self.unit->parent_unit->all_uniforms, sym);
			}
		}
		else if (auto buffer_tag_it = mn::map_lookup(decl->tags.table, KEYWORD_BUFFER))
		{
			if (_typer_check_type_suitable_for_buffer(self, res, 0, false) == false)
			{
				Err err{};
				err.loc = symbol_location(sym);
				err.msg = mn::strf("buffer variable type '{}' contains types which cannot be used in a uniform", *res);
				unit_err(self.unit, err);
			}
			else
			{
				sym->var_sym.is_buffer = true;

				if (auto write_it = mn::map_lookup(decl->tags.table, KEYWORD_READ_WRITE))
					sym->var_sym.is_read_write = true;
				else
					sym->var_sym.is_read_write = false;
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

		_typer_resolve_tags(self, d->tags);

		// TODO: find a nice way to handle the return type of function return type here, for now
		// we set it to void then overwrite it later at the end of this function
		auto scope = unit_create_scope_for(self.unit, d, _typer_current_scope(self), d->name.str, type_void, Scope::FLAG_NONE);
		_typer_enter_scope(self, scope);
		{
			auto type_interner = self.unit->parent_unit->type_interner;
			auto template_args = mn::buf_with_allocator<Type*>(type_interner->arena);
			for (auto template_arg: d->template_args)
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
			for (auto& arg: d->func_decl.args)
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
				_typer_resolve_tags(self, arg.tags);
			}
			sign.return_type = _typer_resolve_type_sign(self, d->func_decl.return_type);
			d->type = type_interner_func(self.unit->parent_unit->type_interner, sign, d, template_args);

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
		mn_assert(sym->kind == Symbol::KIND_FUNC_OVERLOAD_SET);

		auto type = type_interner_overload_set(self.unit->parent_unit->type_interner, sym);
		for (auto& [decl, decl_type]: sym->func_overload_set_sym.decls)
		{
			// enter file scope to make import symbols visible
			_typer_enter_scope(self, decl->loc.file->file_scope);
			mn_defer{_typer_leave_scope(self);};

			_typer_resolve_tags(self, decl->tags);
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
			err.msg = mn::strf("incorrect return type '{}' expected '{}'", *ret, *expected);
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
				err.msg = mn::strf("if condition type '{}' is not a boolean", *cond_type);
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
					err.msg = mn::strf("for loop condition type '{}' is not a boolean", *cond_type);
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
					err.msg = mn::strf("width mismatch in multiply operation '{}' * '{}'", *lhs_type, *rhs_type);
					unit_err(self.unit, err);
				}
			}
			else if (s->assign_stmt.op.kind == Tkn::KIND_PLUS_EQUAL ||
					 s->assign_stmt.op.kind == Tkn::KIND_MINUS_EQUAL ||
					 s->assign_stmt.op.kind == Tkn::KIND_STAR_EQUAL ||
					 s->assign_stmt.op.kind == Tkn::KIND_DIVIDE_EQUAL ||
					 s->assign_stmt.op.kind == Tkn::KIND_MODULUS_EQUAL)
			{
				if (lhs_type->kind == Type::KIND_VEC && type_is_numeric_scalar(rhs_type))
				{
					if (type_is_equal(lhs_type->vec.base, rhs_type))
					{
						// this is allowed
						continue;
					}
					else
					{
						Err err{};
						err.loc = s->loc;
						err.msg = mn::strf("illegal binary operation on vector type, lhs is '{}' and rhs is '{}'", *lhs_type, *rhs_type);
						unit_err(self.unit, err);
					}
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
						err.msg = mn::strf("type '{}' cannot be used in a bitwise shift operation", *rhs_type);
						unit_err(self.unit, err);
					}
					else if (type_width(lhs_type) != type_width(rhs_type))
					{
						Err err{};
						err.loc = s->assign_stmt.rhs[i]->loc;
						err.msg = mn::strf("type '{}' is not compatible with '{}' in a bitwise shift operation", *lhs_type, *rhs_type);
						unit_err(self.unit, err);
					}
				}
				else
				{
					Err err{};
					err.loc = s->assign_stmt.rhs[i]->loc;
					err.msg = mn::strf("type mismatch in assignment statement, expected '{}' but found '{}'", *lhs_type, *rhs_type);
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
			case ADDRESS_MODE_READ_ONLY:
			{
				Err err{};
				err.loc = s->assign_stmt.lhs[i]->loc;
				err.msg = mn::strf("cannot assign into a read only value");
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
			mn_unreachable();
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
			mn_unreachable();
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
	_typer_resolve_func_body_internal(Typer& self, Decl* d, Type* t, Scope* scope)
	{
		if (type_is_templated(t))
			return;

		_typer_enter_scope(self, scope);
		_typer_enter_func(self, d);
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
		_typer_leave_func(self);
		_typer_leave_scope(self);
	}

	inline static void
	_typer_resolve_func_body(Typer& self, Symbol* sym)
	{
		auto d = symbol_decl(sym);
		auto t = sym->type;
		auto scope = unit_create_scope_for(self.unit, d, _typer_current_scope(self), d->name.str, t->as_func.sign.return_type, Scope::FLAG_NONE);
		_typer_resolve_func_body_internal(self, sym->func_sym.decl, sym->type, scope);
	}

	inline static void
	_typer_resolve_func_overload_set_body(Typer& self, Symbol* sym)
	{
		for (auto [decl, decl_type]: sym->func_overload_set_sym.decls)
		{
			auto scope = unit_create_scope_for(self.unit, decl, _typer_current_scope(self), decl->name.str, decl_type->as_func.sign.return_type, Scope::FLAG_NONE);
			_typer_resolve_func_body_internal(self, decl, decl_type, scope);
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
				for (auto template_arg: d->template_args)
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
				for (auto& field: d->struct_decl.fields)
				{
					_typer_resolve_tags(self, field.tags);

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
							err.msg = mn::strf("type mismatch in default value which has type '{}' but field type is '{}'", *default_value_type, *field_type);
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
						err.msg = mn::strf("enum type should be integer, but instead we found '{}'", *value_type);
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
			_typer_add_dependency(self, sym);
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
		mn_defer{
			if (create_sub_typer)
			{
				typer_free(self);
				self = old_typer;
			}
		};

		sym->state = STATE_RESOLVING;

		_typer_add_dependency(self, sym);
		_typer_enter_symbol(self, sym);

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
			_typer_resolve_tags(self, sym->struct_sym.decl->tags);
			break;
		case Symbol::KIND_PACKAGE:
			sym->type = type_interner_package(self.unit->parent_unit->type_interner, sym->package_sym.package);
			_typer_resolve_tags(self, sym->package_sym.decl->tags);
			break;
		case Symbol::KIND_FUNC_OVERLOAD_SET:
			sym->type = _typer_resolve_func_overload_set(self, sym);
			break;
		case Symbol::KIND_ENUM:
			sym->type = type_interner_incomplete(self.unit->parent_unit->type_interner, sym);
			_typer_resolve_tags(self, sym->enum_sym.decl->tags);
			break;
		default:
			mn_unreachable();
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
			mn_unreachable();
			break;
		}

		_typer_leave_symbol(self);

		// if sym is top level we add it to reachable symbols
		sym->is_top_level = scope_is_top_level(self.global_scope, sym);
		if (auto decl = symbol_decl(sym))
			sym->is_top_level |= scope_is_top_level(decl->loc.file->file_scope, sym);

		// we don't prepend scope for local variables
		bool prepend_scope = true;
		if (sym->kind == Symbol::KIND_VAR && sym->is_top_level == false)
			prepend_scope = false;

		sym->package_name = _typer_generate_package_name_for_symbol(self, sym, prepend_scope);

		if (sym->is_top_level ||
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
					err.msg = mn::strf("if condition type '{}' is not a boolean", *cond_type);
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
	_typer_check_entry_struct_input(Typer& self, Type* type, COMPILATION_MODE compilation_mode)
	{
		auto struct_decl = symbol_decl(type->struct_type.symbol);
		size_t struct_type_index = 0;
		for (const auto& field: struct_decl->struct_decl.fields)
		{
			const auto& struct_field = type->struct_type.fields[struct_type_index];

			if (auto tag_it = mn::map_lookup(field.tags.table, KEYWORD_SV_POSITION))
			{
				if (struct_field.type != type_vec4)
				{
					Err err{};
					err.loc = struct_field.name.loc;
					err.msg = mn::strf("system position type is '{}', but it should be 'vec4'", *struct_field.type);
					unit_err(self.unit, err);
				}

				if (compilation_mode != COMPILATION_MODE_VERTEX &&
					compilation_mode != COMPILATION_MODE_PIXEL &&
					compilation_mode != COMPILATION_MODE_GEOMETRY)
				{
					Err err{};
					err.loc = tag_it->value.name.loc;
					err.msg = mn::strf("system position is only available in vertex, pixel, and geometry shaders");
					unit_err(self.unit, err);
				}
			}

			if (auto tag_it = mn::map_lookup(field.tags.table, KEYWORD_SV_DEPTH))
			{
				if (struct_field.type != type_float)
				{
					Err err{};
					err.loc = struct_field.name.loc;
					err.msg = mn::strf("system depth type is '{}', but it should be 'float'", *struct_field.type);
					unit_err(self.unit, err);
				}

				if (compilation_mode != COMPILATION_MODE_PIXEL)
				{
					Err err{};
					err.loc = tag_it->value.name.loc;
					err.msg = mn::strf("system depth is only available in pixel shaders");
					unit_err(self.unit, err);
				}
			}

			if (auto tag_it = mn::map_lookup(field.tags.table, KEYWORD_SV_PRIMITIVE_ID))
			{
				if (struct_field.type != type_uint)
				{
					Err err{};
					err.loc = struct_field.name.loc;
					err.msg = mn::strf("system primitive id type is '{}', but it should be 'uint'", *struct_field.type);
					unit_err(self.unit, err);
				}

				if (compilation_mode != COMPILATION_MODE_VERTEX &&
					compilation_mode != COMPILATION_MODE_PIXEL &&
					compilation_mode != COMPILATION_MODE_GEOMETRY)
				{
					Err err{};
					err.loc = tag_it->value.name.loc;
					err.msg = mn::strf("system primitive id is only available in vertex, pixel, and geometry shaders");
					unit_err(self.unit, err);
				}
			}

			bool is_supported_compute = false;
			if (auto tag_it = mn::map_lookup(field.tags.table, KEYWORD_SV_THREAD_ID))
			{
				is_supported_compute = true;
				if (type_is_equal(struct_field.type, type_uvec3) == false)
				{
					Err err{};
					err.loc = struct_field.name.loc;
					err.msg = mn::strf("system thread id type is '{}', but it should be 'uvec3'", *struct_field.type);
					unit_err(self.unit, err);
				}

				if (compilation_mode != COMPILATION_MODE_COMPUTE)
				{
					Err err{};
					err.loc = tag_it->value.name.loc;
					err.msg = mn::strf("system thread id is only available in compute shaders");
					unit_err(self.unit, err);
				}
			}

			if (auto tag_it = mn::map_lookup(field.tags.table, KEYWORD_SV_GROUP_ID))
			{
				is_supported_compute = true;
				if (type_is_equal(struct_field.type, type_uvec3) == false)
				{
					Err err{};
					err.loc = struct_field.name.loc;
					err.msg = mn::strf("system group id type is '{}', but it should be 'uvec3'", *struct_field.type);
					unit_err(self.unit, err);
				}

				if (compilation_mode != COMPILATION_MODE_COMPUTE)
				{
					Err err{};
					err.loc = tag_it->value.name.loc;
					err.msg = mn::strf("system group id is only available in compute shaders");
					unit_err(self.unit, err);
				}
			}

			if (compilation_mode == COMPILATION_MODE_COMPUTE)
			{
				if (is_supported_compute == false)
				{
					Err err{};
					err.loc = struct_field.name.loc;
					err.msg = mn::strf("compute shader only supports 'system_thread_id' arguments");
					unit_err(self.unit, err);
				}
			}
			else
			{
				if (type_is_shader_api(struct_field.type, SHADER_API_DEFAULT) == false)
				{
					Err err{};
					err.loc = struct_field.name.loc;
					err.msg = mn::strf("type '{}' cannot be used as shader input", *struct_field.type);
					unit_err(self.unit, err);
				}
			}

			struct_type_index += field.names.count;
		}
	}

	inline static void
	_typer_check_entry_input(Typer& self, Entry_Point* entry)
	{
		auto decl = symbol_decl(entry->symbol);
		auto type = entry->symbol->type;

		if (auto tag_it = mn::map_lookup(decl->tags.table, KEYWORD_GEOMETRY))
		{
			if (mn::map_lookup(tag_it->value.args, KEYWORD_MAX_VERTEX_COUNT) == nullptr)
			{
				Err err{};
				err.loc = decl->loc;
				err.msg = mn::strf("geometry shader should have max vertex count tag argument '@geometry{{max_vertex_count = 6, ...}}'");
				unit_err(self.unit, err);
			}
		}

		size_t type_index = 0;
		for (auto arg: decl->func_decl.args)
		{
			auto arg_type = type->as_func.sign.args.types[type_index];

			if (arg_type->kind == Type::KIND_STRUCT)
			{
				_typer_check_entry_struct_input(self, arg_type, entry->mode);
				type_index += arg.names.count;
				continue;
			}
			else if (arg_type->kind == Type::KIND_ARRAY)
			{
				auto base_type = arg_type->array.base;
				if (base_type->kind == Type::KIND_STRUCT)
				{
					_typer_check_entry_struct_input(self, base_type, entry->mode);
					type_index += arg.names.count;
					continue;
				}
			}

			Location err_loc{};
			if (arg.type.atoms.count > 0)
				err_loc = mn::buf_top(arg.type.atoms).named.type_name.loc;
			else if (arg.names.count > 0)
				err_loc = arg.names[0].loc;

			if (auto tag_it = mn::map_lookup(arg.tags.table, KEYWORD_SV_THREAD_ID))
			{
				if (type_is_equal(arg_type, type_uvec3) == false)
				{
					Err err{};
					err.loc = err_loc;
					err.msg = mn::strf("system thread id type is '{}', but it should be 'uvec3'", *arg_type);
					unit_err(self.unit, err);
					continue;
				}

				if (entry->mode != COMPILATION_MODE_COMPUTE)
				{
					Err err{};
					err.loc = tag_it->value.name.loc;
					err.msg = mn::strf("system thread id is only available in compute shaders");
					unit_err(self.unit, err);
					continue;
				}
			}
			else if (auto tag_it = mn::map_lookup(arg.tags.table, KEYWORD_SV_GROUP_ID))
			{
				if (type_is_equal(arg_type, type_uvec3) == false)
				{
					Err err{};
					err.loc = err_loc;
					err.msg = mn::strf("system group id type is '{}', but it should be 'uvec3'", *arg_type);
					unit_err(self.unit, err);
					continue;
				}

				if (entry->mode != COMPILATION_MODE_COMPUTE)
				{
					Err err{};
					err.loc = tag_it->value.name.loc;
					err.msg = mn::strf("system group id is only available in compute shaders");
					unit_err(self.unit, err);
					continue;
				}
			}
			else
			{
				if (entry->mode == COMPILATION_MODE_COMPUTE)
				{
					Err err{};
					err.loc = err_loc;
					err.msg = mn::strf("compute shader only supports 'system_thread_id', and 'system_group_id' arguments");
					unit_err(self.unit, err);
					continue;
				}
			}

			int api_config = SHADER_API_DEFAULT;
			if (entry->mode == COMPILATION_MODE_GEOMETRY)
				api_config |= SHADER_API_ALLOW_STREAMS;

			if (type_is_shader_api(arg_type, api_config) == false)
			{
				Err err{};
				err.loc = err_loc;
				err.msg = mn::strf("type '{}' cannot be used as shader input", *arg_type);
				unit_err(self.unit, err);
			}
			type_index += arg.names.count;
		}

		auto return_type = type->as_func.sign.return_type;

		// special case geometry shaders
		if (entry->mode == COMPILATION_MODE_GEOMETRY)
		{
			if (return_type != type_void)
			{
				Err err{};
				err.loc = decl->loc;
				err.msg = mn::strf("geometry shader return type should be void, but found '{}'", *return_type);
				unit_err(self.unit, err);
			}
		}

		// special case compute shaders
		if (entry->mode == COMPILATION_MODE_COMPUTE)
		{
			if (return_type != type_void)
			{
				Err err{};
				err.loc = decl->loc;
				err.msg = mn::strf("compute shader return type should be void, but found '{}'", *return_type);
				unit_err(self.unit, err);
			}
		}

		// handle return type
		if (return_type->kind == Type::KIND_STRUCT)
		{
			_typer_check_entry_struct_input(self, return_type, entry->mode);
		}
		else
		{
			Location err_loc{};
			if (decl->func_decl.return_type.atoms.count > 0)
				err_loc = mn::buf_top(decl->func_decl.return_type.atoms).named.type_name.loc;

			int api_config = SHADER_API_DEFAULT;
			if (entry->mode == COMPILATION_MODE_GEOMETRY)
				api_config |= SHADER_API_ALLOW_VOID;
			else if (entry->mode == COMPILATION_MODE_COMPUTE)
				api_config |= SHADER_API_ALLOW_VOID;

			if (type_is_shader_api(return_type, api_config) == false)
			{
				Err err{};
				err.loc = err_loc;
				err.msg = mn::strf("type '{}' cannot be used as shader output", *return_type);
				unit_err(self.unit, err);
			}
		}
	}

	inline static void
	_typer_assign_bindings(Typer& self, Entry_Point* entry, Symbol* sym)
	{
		mn_assert(sym->kind == Symbol::KIND_VAR && sym->var_sym.is_uniform);
		if (sym->var_sym.uniform_binding_processed)
		{
			if (entry)
			{
				if (sym->type->kind == Type::KIND_TEXTURE)
				{
					mn::buf_push(entry->textures, sym);
				}
				else if (type_is_sampler(sym->type))
				{
					mn::buf_push(entry->samplers, sym);
				}
				else
				{
					mn::buf_push(entry->uniforms, sym);
				}
			}
			return;
		}

		sym->var_sym.uniform_binding_processed = true;
		auto decl = symbol_decl(sym);
		auto uniform_tag_it = mn::map_lookup(decl->tags.table, KEYWORD_UNIFORM);
		if (sym->type->kind == Type::KIND_TEXTURE)
		{
			if (auto binding_it = mn::map_lookup(uniform_tag_it->value.args, KEYWORD_BINDING))
			{
				auto value_expr = binding_it->value.value;
				if (value_expr->mode == ADDRESS_MODE_CONST &&
					value_expr->const_value.type == type_int)
				{
					sym->var_sym.uniform_binding = value_expr->const_value.as_int;
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
				if (entry)
					mn::buf_push(entry->textures, sym);
			}
		}
		else if (type_is_sampler(sym->type))
		{
			if (auto binding_it = mn::map_lookup(uniform_tag_it->value.args, KEYWORD_BINDING))
			{
				auto value_expr = binding_it->value.value;
				if (value_expr->mode == ADDRESS_MODE_CONST &&
					value_expr->const_value.type == type_int)
				{
					sym->var_sym.uniform_binding = value_expr->const_value.as_int;
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
				if (entry)
					mn::buf_push(entry->samplers, sym);
			}
		}
		else
		{
			if (auto binding_it = mn::map_lookup(uniform_tag_it->value.args, KEYWORD_BINDING))
			{
				auto value_expr = binding_it->value.value;
				if (value_expr->mode == ADDRESS_MODE_CONST &&
					value_expr->const_value.type == type_int)
				{
					sym->var_sym.uniform_binding = value_expr->const_value.as_int;
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
				if (entry)
					mn::buf_push(entry->uniforms, sym);
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
		mn::buf_free(self.func_stack);
	}

	void
	typer_check(Typer& self)
	{
		_typer_shallow_walk(self);

		for (auto sym: self.unit->global_scope->symbols)
		{
			if (sym->kind == Symbol::KIND_FUNC)
			{
				auto decl = symbol_decl(sym);
				if (mn::map_lookup(decl->tags.table, KEYWORD_VERTEX) != nullptr)
				{
					auto entry = entry_point_new(sym, COMPILATION_MODE_VERTEX);
					mn::buf_push(self.unit->entry_points, entry);
				}
				else if (mn::map_lookup(decl->tags.table, KEYWORD_PIXEL) != nullptr)
				{
					auto entry = entry_point_new(sym, COMPILATION_MODE_PIXEL);
					mn::buf_push(self.unit->entry_points, entry);
				}
				else if (auto tag_it = mn::map_lookup(decl->tags.table, KEYWORD_GEOMETRY))
				{
					auto entry = entry_point_new(sym, COMPILATION_MODE_GEOMETRY);
					mn::buf_push(self.unit->entry_points, entry);
				}
				else if (auto tag_it = mn::map_lookup(decl->tags.table, KEYWORD_COMPUTE))
				{
					auto entry = entry_point_new(sym, COMPILATION_MODE_COMPUTE);
					mn::buf_push(self.unit->entry_points, entry);
				}
			}
		}

		// check all symbols
		for (auto sym: self.global_scope->symbols)
			_typer_resolve_symbol(self, sym);

		// handle binding points
		auto visited = mn::set_with_allocator<Symbol*>(mn::memory::tmp());
		auto stack = mn::buf_with_allocator<Symbol*>(mn::memory::tmp());
		for (auto entry: self.unit->entry_points)
		{
			mn::set_clear(visited);
			mn::buf_clear(stack);

			auto sym = entry->symbol;
			mn::set_insert(visited, sym);
			mn::buf_push(stack, sym);
			while (stack.count > 0)
			{
				auto sym = mn::buf_top(stack);
				mn::buf_pop(stack);

				// process symbol here
				if (sym->kind == Symbol::KIND_VAR && sym->var_sym.is_uniform)
				{
					_typer_assign_bindings(self, entry, sym);
				}

				for (auto d: sym->dependencies)
				{
					if (mn::set_lookup(visited, d) == nullptr)
					{
						mn::buf_push(stack, d);
						mn::set_insert(visited, d);
					}
				}
			}
		}

		for (auto sym: self.unit->parent_unit->all_uniforms)
			_typer_assign_bindings(self, nullptr, sym);
	}

	void
	typer_check_entry(Typer& self, Entry_Point* entry)
	{
		_typer_check_entry_input(self, entry);
	}
}