#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"
#include "sabre/AST.h"

#include <mn/Memory.h>
#include <mn/Buf.h>
#include <mn/Map.h>
#include <mn/Fmt.h>

namespace sabre
{
	struct Type;

	// describes a function signature
	struct Func_Sign
	{
		mn::Buf<Type*> args;
		Type* return_type;

		bool
		operator==(const Func_Sign& other) const
		{
			if (args.count != other.args.count)
				return false;

			for (size_t i = 0; i < args.count; ++i)
				if (args[i] != other.args[i])
					return false;

			if (return_type != other.return_type)
				return false;

			return true;
		}

		bool
		operator!=(const Func_Sign& other) const
		{
			return !operator==(other);
		}
	};

	// creates a new function signature
	inline static Func_Sign
	func_sign_new()
	{
		return Func_Sign{};
	}

	// frees the given function signature
	inline static void
	func_sign_free(Func_Sign& self)
	{
		mn::buf_free(self.args);
	}

	inline static void
	destruct(Func_Sign& self)
	{
		func_sign_free(self);
	}

	// used to hash a function signature
	struct Func_Sign_Hasher
	{
		inline size_t
		operator()(const Func_Sign& sign) const
		{
			return mn::hash_mix(mn::murmur_hash(block_from(sign.args)), mn::Hash<Type*>()(sign.return_type));
		}
	};

	// represents a data type
	struct Type
	{
		enum KIND
		{
			KIND_VOID,
			KIND_BOOL,
			KIND_INT,
			KIND_UINT,
			KIND_FLOAT32,
			KIND_FLOAT64,
			KIND_FUNC,
		};

		KIND kind;
		union
		{
			Func_Sign func;
		};
	};

	// language basic datatypes
	SABRE_EXPORT extern Type* type_void;
	SABRE_EXPORT extern Type* type_bool;
	SABRE_EXPORT extern Type* type_int;
	SABRE_EXPORT extern Type* type_uint;
	SABRE_EXPORT extern Type* type_float32;
	SABRE_EXPORT extern Type* type_float64;

	inline static Type*
	type_from_name(Tkn name)
	{
		if (name.kind != Tkn::KIND_ID)
			return type_void;

		auto str = mn::str_lit(name.str);
		if (str == "bool")
			return type_bool;
		else if (str == "int")
			return type_int;
		else if (str == "uint")
			return type_uint;
		else if (str == "float32")
			return type_float32;
		else if (str == "float64")
			return type_float64;
		else
			return type_void;
	}

	inline static bool
	type_is_equal(Type* a, Type* b)
	{
		return a == b;
	}

	// interns the different types to make comparisons and memory management easier
	struct Type_Interner
	{
		mn::Allocator arena;
		mn::Map<Func_Sign, Type*, Func_Sign_Hasher> func_table;
	};

	// creates a new type interner
	SABRE_EXPORT Type_Interner
	type_interner_new();

	// frees the given type interner
	SABRE_EXPORT void
	type_interner_free(Type_Interner& self);

	inline static void
	destruct(Type_Interner& self)
	{
		type_interner_free(self);
	}

	// interns a function signature into a type, it will consume the given function signature
	SABRE_EXPORT Type*
	type_interner_func(Type_Interner& self, Func_Sign sign);

	// represents a symbol in the code
	struct Symbol
	{
		enum KIND
		{
			KIND_CONST,
			KIND_VAR,
			KIND_FUNC,
		};

		enum STATE
		{
			STATE_UNRESOLVED,
			STATE_RESOLVING,
			STATE_RESOLVED,
		};

		KIND kind;
		STATE state;
		Type* type;
		const char* name;

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
			} var_sym;

			struct
			{
				Decl* decl;
				Tkn name;
			} func_sym;
		};
	};

	// creates a new symbol for a constant declaration
	inline static Symbol*
	symbol_const_new(mn::Allocator arena, Tkn name, Decl* decl, Type_Sign sign, Expr* value)
	{
		auto self = mn::alloc_zerod_from<Symbol>(arena);
		self->kind = Symbol::KIND_CONST;
		self->state = Symbol::STATE_UNRESOLVED;
		self->type = type_void;
		self->name = name.str;
		self->const_sym.decl = decl;
		self->const_sym.name = name;
		self->const_sym.sign = sign;
		self->const_sym.value = value;
		return self;
	}

	// creates a new symbol for a variable declaration
	inline static Symbol*
	symbol_var_new(mn::Allocator arena, Tkn name, Decl* decl, Type_Sign sign, Expr* value)
	{
		auto self = mn::alloc_zerod_from<Symbol>(arena);
		self->kind = Symbol::KIND_VAR;
		self->state = Symbol::STATE_UNRESOLVED;
		self->type = type_void;
		self->name = name.str;
		self->var_sym.decl = decl;
		self->var_sym.name = name;
		self->var_sym.sign = sign;
		self->var_sym.value = value;
		return self;
	}

	// creates a new symbol for a function declaration
	inline static Symbol*
	symbol_func_new(mn::Allocator arena, Tkn name, Decl* decl)
	{
		auto self = mn::alloc_zerod_from<Symbol>(arena);
		self->kind = Symbol::KIND_FUNC;
		self->state = Symbol::STATE_UNRESOLVED;
		self->type = type_void;
		self->name = name.str;
		self->func_sym.decl = decl;
		self->func_sym.name = name;
		return self;
	}

	// given a symbols it will return its location in compilation unit
	inline static Location
	symbol_location(const Symbol* self)
	{
		switch (self->kind)
		{
		case Symbol::KIND_CONST:
			return self->const_sym.decl->loc;
		case Symbol::KIND_VAR:
			return self->var_sym.decl->loc;
		case Symbol::KIND_FUNC:
			return self->func_sym.decl->loc;
		default:
			assert(false && "unreachable");
			return Location{};
		}
	}

	// scope contains symbols inside a scope node in the AST (like a func)
	struct Scope
	{
		Scope* parent;
		const char* name;
		mn::Buf<Symbol*> symbols;
		mn::Map<const char*, Symbol*> symbol_table;
	};

	// creates a new scope
	SABRE_EXPORT Scope*
	scope_new(Scope* parent, const char* name);

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
}

namespace fmt
{
	template<>
	struct formatter<sabre::Type*> {
		template <typename ParseContext>
		constexpr auto parse(ParseContext &ctx) { return ctx.begin(); }

		template <typename FormatContext>
		auto format(const sabre::Type* t, FormatContext &ctx) {
			if (t == sabre::type_void)
			{
				return format_to(ctx.out(), "void");
			}
			else if (t == sabre::type_bool)
			{
				return format_to(ctx.out(), "bool");
			}
			else if (t == sabre::type_int)
			{
				return format_to(ctx.out(), "int");
			}
			else if (t == sabre::type_uint)
			{
				return format_to(ctx.out(), "uint");
			}
			else if (t == sabre::type_float32)
			{
				return format_to(ctx.out(), "float32");
			}
			else if (t == sabre::type_float64)
			{
				return format_to(ctx.out(), "float64");
			}
			else
			{
				assert(false && "unreachable");
				return format_to(ctx.out(), "<UNKNOWN TYPE>");
			}
		}
	};
}