#pragma once

#include "sabre/Exports.h"
#include "sabre/AST.h"

#include <mn/Map.h>
#include <mn/Assert.h>

namespace sabre
{
	struct Type;
	struct Unit_Package;
	struct Scope;

	// represent the stages of resolving a value, useful for detecting cyclic dependencies
	enum STATE
	{
		STATE_UNRESOLVED,
		STATE_RESOLVING,
		STATE_RESOLVED,
	};

	// represents a symbol in the code
	struct Symbol
	{
		enum KIND
		{
			KIND_CONST,
			KIND_VAR,
			KIND_FUNC,
			KIND_STRUCT,
			KIND_PACKAGE,
			KIND_FUNC_OVERLOAD_SET,
			KIND_ENUM,
			KIND_TYPENAME,
			KIND_STRUCT_INSTANTIATION,
			KIND_FUNC_INSTANTIATION,
		};

		KIND kind;
		STATE state;
		Type* type;
		Unit_Package* package;
		Scope* scope;
		const char* name;
		const char* package_name;
		mn::Set<Symbol*> dependencies;
		bool is_top_level;

		union
		{
			struct
			{
				Decl* decl;
				Tkn name;
				Type_Sign sign;
				Expr* value;
			} const_sym;

			struct
			{
				Decl* decl;
				Tkn name;
				Type_Sign sign;
				Expr* value;

				// used when a variable refers is a uniform
				int uniform_binding;
				bool is_uniform;
				bool uniform_binding_processed;
			} var_sym;

			struct
			{
				Decl* decl;
				Tkn name;
			} func_sym;

			struct
			{
				Decl* decl;
				Tkn name;
			} struct_sym;

			struct
			{
				Decl* decl;
			} enum_sym;

			struct
			{
				Decl* decl;
				Tkn name;
				Unit_Package* package;
			} package_sym;

			struct
			{
				mn::Map<Decl*, Type*> decls;
				mn::Buf<Decl*> used_decls;
				mn::Set<Decl*> unique_used_decls;
			} func_overload_set_sym;

			struct
			{
				Tkn name;
			} typename_sym;

			struct
			{
				Symbol* template_symbol;
			} as_struct_instantiation;

			struct
			{
				Symbol* template_symbol;
				Decl* decl;
			} as_func_instantiation;
		};
	};

	// creates a new symbol for a constant declaration
	SABRE_EXPORT Symbol*
	symbol_const_new(mn::Allocator arena, Tkn name, Decl* decl, Type_Sign sign, Expr* value);

	// creates a new symbol for a variable declaration
	SABRE_EXPORT Symbol*
	symbol_var_new(mn::Allocator arena, Tkn name, Decl* decl, Type_Sign sign, Expr* value);

	// creates a new symbol for a function declaration
	SABRE_EXPORT Symbol*
	symbol_func_new(mn::Allocator arena, Tkn name, Decl* decl);

	// creates a new symbol for a struct declaration
	SABRE_EXPORT Symbol*
	symbol_struct_new(mn::Allocator arena, Tkn name, Decl* decl);

	// creates a new symbol for enum declaration
	SABRE_EXPORT Symbol*
	symbol_enum_new(mn::Allocator arena, Tkn name, Decl* decl);

	// creates a new symbol for package declaration
	SABRE_EXPORT Symbol*
	symbol_package_new(mn::Allocator arena, Tkn name, Decl* decl, Unit_Package* package);

	// creates a new symbol for function overload set given a function
	SABRE_EXPORT Symbol*
	symbol_func_overload_set_new(mn::Allocator arena, Symbol* func);

	// creates a new symbol for a typename, usually this is a template argument to some struct, func, etc...
	SABRE_EXPORT Symbol*
	symbol_typename_new(mn::Allocator arena, Tkn name);

	// creates a new symbol for a struct instantiation
	SABRE_EXPORT Symbol*
	symbol_struct_instantiation_new(mn::Allocator arena, Symbol* template_symbol, Type* type);

	// creates a new symbol for a func instantiation
	SABRE_EXPORT Symbol*
	symbol_func_instantiation_new(mn::Allocator arena, Symbol* template_symbol, Type* type, Decl* decl);

	// given a symbols it will return its location in compilation unit
	inline static Location
	symbol_location(const Symbol* self)
	{
		switch (self->kind)
		{
		case Symbol::KIND_CONST:
			return self->const_sym.decl->loc;
		case Symbol::KIND_VAR:
			if (self->var_sym.decl)
				return self->var_sym.decl->loc;
			else
				return self->var_sym.name.loc;
		case Symbol::KIND_FUNC:
			return self->func_sym.decl->loc;
		case Symbol::KIND_STRUCT:
			return self->struct_sym.decl->loc;
		case Symbol::KIND_PACKAGE:
			return self->package_sym.decl->loc;
		case Symbol::KIND_ENUM:
			return self->enum_sym.decl->loc;
		case Symbol::KIND_TYPENAME:
			return self->typename_sym.name.loc;
		case Symbol::KIND_STRUCT_INSTANTIATION:
			return symbol_location(self->as_struct_instantiation.template_symbol);
		default:
			mn_unreachable();
			return Location{};
		}
	}

	// given a symbol it will return the declaration it represents
	inline static Decl*
	symbol_decl(const Symbol* self)
	{
		switch (self->kind)
		{
		case Symbol::KIND_CONST:
			return self->const_sym.decl;
		case Symbol::KIND_VAR:
			return self->var_sym.decl;
		case Symbol::KIND_FUNC:
			return self->func_sym.decl;
		case Symbol::KIND_STRUCT:
			return self->struct_sym.decl;
		case Symbol::KIND_PACKAGE:
			return self->package_sym.decl;
		case Symbol::KIND_FUNC_OVERLOAD_SET:
			return nullptr;
		case Symbol::KIND_ENUM:
			return self->enum_sym.decl;
		case Symbol::KIND_TYPENAME:
			return nullptr;
		case Symbol::KIND_STRUCT_INSTANTIATION:
			return symbol_decl(self->as_struct_instantiation.template_symbol);
		default:
			mn_unreachable();
			return nullptr;
		}
	}

	// scope contains symbols inside a scope node in the AST (like a func)
	struct Scope
	{
		enum FLAG
		{
			FLAG_NONE,
			FLAG_INSIDE_LOOP,
		};

		Scope* parent;
		const char* name;
		mn::Buf<Symbol*> symbols;
		mn::Map<const char*, Symbol*> symbol_table;
		Type* expected_type;
		mn::Map<const char*, size_t> generated_names;
		FLAG flags;
	};

	// creates a new scope
	SABRE_EXPORT Scope*
	scope_new(Scope* parent, const char* name, Type* expected_type, Scope::FLAG flags);

	// frees the given scope
	SABRE_EXPORT void
	scope_free(Scope* self);

	inline static void
	destruct(Scope* self)
	{
		scope_free(self);
	}

	// search the given scope only for a symbol with the given name
	inline static Symbol*
	scope_shallow_find(const Scope* self, const char* name)
	{
		if (auto it = mn::map_lookup(self->symbol_table, name))
			return it->value;
		return nullptr;
	}

	inline static bool
	scope_add(Scope* self, Symbol* symbol)
	{
		if (scope_shallow_find(self, symbol->name) != nullptr)
			return false;

		mn::map_insert(self->symbol_table, symbol->name, symbol);
		mn::buf_push(self->symbols, symbol);
		return true;
	}

	inline static Symbol*
	scope_find(const Scope* self, const char* name)
	{
		for (auto it = self; it != nullptr; it = it->parent)
		{
			if (auto sym = scope_shallow_find(it, name))
				return sym;
		}
		return nullptr;
	}

	inline static bool
	scope_is_top_level(Scope* self, Symbol* symbol)
	{
		for (auto sym: self->symbols)
			if (sym == symbol)
				return true;
		return false;
	}

	inline static bool
	scope_find_flag(Scope* self, Scope::FLAG flag)
	{
		for (auto it = self; it != nullptr; it = it->parent)
			if ((it->flags & flag) != 0)
				return true;
		return false;
	}
}