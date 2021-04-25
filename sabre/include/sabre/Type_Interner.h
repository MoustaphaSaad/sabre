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
			KIND_FLOAT,
			KIND_DOUBLE,
			KIND_VEC,
			KIND_FUNC,
		};

		KIND kind;
		union
		{
			Func_Sign func;
			struct
			{
				Type* base;
				int width;
			} vec;
		};
	};

	// language basic datatypes
	SABRE_EXPORT extern Type* type_void;
	SABRE_EXPORT extern Type* type_bool;
	SABRE_EXPORT extern Type* type_int;
	SABRE_EXPORT extern Type* type_uint;
	SABRE_EXPORT extern Type* type_float;
	SABRE_EXPORT extern Type* type_double;
	SABRE_EXPORT extern Type* type_vec2;
	SABRE_EXPORT extern Type* type_vec3;
	SABRE_EXPORT extern Type* type_vec4;
	SABRE_EXPORT extern Type* type_bvec2;
	SABRE_EXPORT extern Type* type_bvec3;
	SABRE_EXPORT extern Type* type_bvec4;
	SABRE_EXPORT extern Type* type_ivec2;
	SABRE_EXPORT extern Type* type_ivec3;
	SABRE_EXPORT extern Type* type_ivec4;
	SABRE_EXPORT extern Type* type_uvec2;
	SABRE_EXPORT extern Type* type_uvec3;
	SABRE_EXPORT extern Type* type_uvec4;
	SABRE_EXPORT extern Type* type_dvec2;
	SABRE_EXPORT extern Type* type_dvec3;
	SABRE_EXPORT extern Type* type_dvec4;

	// given a type name it will return a type
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
		else if (str == "float")
			return type_float;
		else if (str == "double")
			return type_double;
		else if (str == "vec2")
			return type_vec2;
		else if (str == "vec3")
			return type_vec3;
		else if (str == "vec4")
			return type_vec4;
		else if (str == "bvec2")
			return type_bvec2;
		else if (str == "bvec3")
			return type_bvec3;
		else if (str == "bvec4")
			return type_bvec4;
		else if (str == "ivec2")
			return type_ivec2;
		else if (str == "ivec3")
			return type_ivec3;
		else if (str == "ivec4")
			return type_ivec4;
		else if (str == "uvec2")
			return type_uvec2;
		else if (str == "uvec3")
			return type_uvec3;
		else if (str == "uvec4")
			return type_uvec4;
		else if (str == "dvec2")
			return type_dvec2;
		else if (str == "dvec3")
			return type_dvec3;
		else if (str == "dvec4")
			return type_dvec4;
		else
			return type_void;
	}

	// returns whether two types are equal
	inline static bool
	type_is_equal(Type* a, Type* b)
	{
		return a == b;
	}

	// returns whether a type can use ++v, --v, +v, -v, etc...
	inline static bool
	type_is_numeric(Type* a)
	{
		return (
			type_is_equal(a, type_int) ||
			type_is_equal(a, type_uint) ||
			type_is_equal(a, type_float) ||
			type_is_equal(a, type_double)
		);
	}

	// returns whether a type is a functions
	inline static bool
	type_is_func(Type* a)
	{
		return a->kind == Type::KIND_FUNC;
	}

	// returns whether a type has a boolean behavior
	inline static bool
	type_is_bool_like(Type* a)
	{
		if (a == type_bool)
			return true;
		else if (a->kind == Type::KIND_VEC && a->vec.base == type_bool)
			return true;
		return false;
	}

	// creates a new vector type, max width == 4
	inline static Type*
	type_vectorize(Type* base, int width)
	{
		if (width == 1)
			return base;

		if (base == type_float)
		{
			switch (width)
			{
			case 2: return type_vec2;
			case 3: return type_vec3;
			case 4: return type_vec4;
			default:
				assert(false && "unreachable");
				return type_void;
			}
		}
		else if (base == type_bool)
		{
			switch (width)
			{
			case 2: return type_bvec2;
			case 3: return type_bvec3;
			case 4: return type_bvec4;
			default:
				assert(false && "unreachable");
				return type_void;
			}
		}
		else if (base == type_int)
		{
			switch (width)
			{
			case 2: return type_ivec2;
			case 3: return type_ivec3;
			case 4: return type_ivec4;
			default:
				assert(false && "unreachable");
				return type_void;
			}
		}
		else if (base == type_uint)
		{
			switch (width)
			{
			case 2: return type_uvec2;
			case 3: return type_uvec3;
			case 4: return type_uvec4;
			default:
				assert(false && "unreachable");
				return type_void;
			}
		}
		else if (base == type_double)
		{
			switch (width)
			{
			case 2: return type_dvec2;
			case 3: return type_dvec3;
			case 4: return type_dvec4;
			default:
				assert(false && "unreachable");
				return type_void;
			}
		}
		return type_void;
	}

	// interns the different types to make comparisons and memory management easier
	struct Type_Interner
	{
		mn::memory::Arena* arena;
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
			else if (t == sabre::type_float)
			{
				return format_to(ctx.out(), "float");
			}
			else if (t == sabre::type_double)
			{
				return format_to(ctx.out(), "double");
			}
			else if (t == sabre::type_vec2)
			{
				return format_to(ctx.out(), "vec2");
			}
			else if (t == sabre::type_vec3)
			{
				return format_to(ctx.out(), "vec3");
			}
			else if (t == sabre::type_vec4)
			{
				return format_to(ctx.out(), "vec4");
			}
			else if (t == sabre::type_bvec2)
			{
				return format_to(ctx.out(), "bvec2");
			}
			else if (t == sabre::type_bvec3)
			{
				return format_to(ctx.out(), "bvec3");
			}
			else if (t == sabre::type_bvec4)
			{
				return format_to(ctx.out(), "bvec4");
			}
			else if (t == sabre::type_ivec2)
			{
				return format_to(ctx.out(), "ivec2");
			}
			else if (t == sabre::type_ivec3)
			{
				return format_to(ctx.out(), "ivec3");
			}
			else if (t == sabre::type_ivec4)
			{
				return format_to(ctx.out(), "ivec4");
			}
			else if (t == sabre::type_uvec2)
			{
				return format_to(ctx.out(), "uvec2");
			}
			else if (t == sabre::type_uvec3)
			{
				return format_to(ctx.out(), "uvec3");
			}
			else if (t == sabre::type_uvec4)
			{
				return format_to(ctx.out(), "uvec4");
			}
			else if (t == sabre::type_dvec2)
			{
				return format_to(ctx.out(), "dvec2");
			}
			else if (t == sabre::type_dvec3)
			{
				return format_to(ctx.out(), "dvec3");
			}
			else if (t == sabre::type_dvec4)
			{
				return format_to(ctx.out(), "dvec4");
			}
			else
			{
				assert(false && "unreachable");
				return format_to(ctx.out(), "<UNKNOWN TYPE>");
			}
		}
	};
}