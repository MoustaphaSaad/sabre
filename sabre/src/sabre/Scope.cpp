#include "sabre/Scope.h"
#include "sabre/Type_Interner.h"

namespace sabre
{
	// API
	Symbol*
	symbol_const_new(mn::Allocator arena, Tkn name, Decl* decl, Type_Sign sign, Expr* value)
	{
		auto self = mn::alloc_zerod_from<Symbol>(arena);
		self->kind = Symbol::KIND_CONST;
		self->state = STATE_UNRESOLVED;
		self->type = type_void;
		self->name = name.str;
		self->const_sym.decl = decl;
		self->const_sym.name = name;
		self->const_sym.sign = sign;
		self->const_sym.value = value;
		return self;
	}

	Symbol*
	symbol_var_new(mn::Allocator arena, Tkn name, Decl* decl, Type_Sign sign, Expr* value)
	{
		auto self = mn::alloc_zerod_from<Symbol>(arena);
		self->kind = Symbol::KIND_VAR;
		self->state = STATE_UNRESOLVED;
		self->type = type_void;
		self->name = name.str;
		self->var_sym.decl = decl;
		self->var_sym.name = name;
		self->var_sym.sign = sign;
		self->var_sym.value = value;
		return self;
	}

	Symbol*
	symbol_func_new(mn::Allocator arena, Tkn name, Decl* decl)
	{
		auto self = mn::alloc_zerod_from<Symbol>(arena);
		self->kind = Symbol::KIND_FUNC;
		self->state = STATE_UNRESOLVED;
		self->type = type_void;
		self->name = name.str;
		self->func_sym.decl = decl;
		self->func_sym.name = name;
		return self;
	}

	Symbol*
	symbol_struct_new(mn::Allocator arena, Tkn name, Decl* decl)
	{
		auto self = mn::alloc_zerod_from<Symbol>(arena);
		self->kind = Symbol::KIND_STRUCT;
		self->state = STATE_UNRESOLVED;
		self->type = type_void;
		self->name = name.str;
		self->struct_sym.decl = decl;
		self->struct_sym.name = name;
		return self;
	}

	Symbol*
	symbol_enum_new(mn::Allocator arena, Tkn name, Decl* decl)
	{
		auto self = mn::alloc_zerod_from<Symbol>(arena);
		self->kind = Symbol::KIND_ENUM;
		self->state = STATE_UNRESOLVED;
		self->type = type_void;
		self->name = name.str;
		self->enum_sym.decl = decl;
		return self;
	}

	Symbol*
	symbol_package_new(mn::Allocator arena, Tkn name, Decl* decl, Unit_Package* package)
	{
		auto self = mn::alloc_zerod_from<Symbol>(arena);
		self->kind = Symbol::KIND_PACKAGE;
		self->state = STATE_UNRESOLVED;
		self->type = type_void;
		self->name = name.str;
		self->package_sym.decl = decl;
		self->package_sym.name = name;
		self->package_sym.package = package;
		return self;
	}

	Symbol*
	symbol_func_overload_set_new(mn::Allocator arena, Symbol* func)
	{
		auto func_decl = func->func_sym.decl;
		auto func_name = func->func_sym.name;
		auto func_type = func->type;
		auto func_used = func->state == STATE_RESOLVED;

		auto self = func;
		self->kind = Symbol::KIND_FUNC_OVERLOAD_SET;
		self->state = STATE_UNRESOLVED;
		self->type = type_void;
		self->func_overload_set_sym.decls = mn::map_with_allocator<Decl*, Type*>(arena);
		self->func_overload_set_sym.used_decls = mn::buf_with_allocator<Decl*>(arena);
		self->func_overload_set_sym.unique_used_decls = mn::set_with_allocator<Decl*>(arena);
		mn::map_insert(self->func_overload_set_sym.decls, func_decl, func_type);
		if (func_used)
		{
			mn::buf_push(self->func_overload_set_sym.used_decls, func_decl);
			mn::set_insert(self->func_overload_set_sym.unique_used_decls, func_decl);
		}
		return self;
	}

	Scope*
	scope_new(Scope* parent, const char* name, Type* expected_type, Scope::FLAG flags)
	{
		auto self = mn::alloc_zerod<Scope>();
		self->parent = parent;
		self->name = name;
		self->expected_type = expected_type;
		self->flags = flags;
		return self;
	}

	void
	scope_free(Scope* self)
	{
		if (self)
		{
			mn::buf_free(self->symbols);
			mn::map_free(self->symbol_table);
			mn::map_free(self->generated_names);
			mn::free(self);
		}
	}
}