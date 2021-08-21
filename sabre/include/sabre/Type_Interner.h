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
	struct Symbol;
	struct Unit_Package;
	struct Scope;

	// describes a function argument signature
	struct Func_Args_Sign
	{
		mn::Buf<Type*> types;

		bool
		operator==(const Func_Args_Sign& other) const
		{
			if (types.count != other.types.count)
				return false;

			for (size_t i = 0; i < types.count; ++i)
				if (types[i] != other.types[i])
					return false;

			return true;
		}

		bool
		operator!=(const Func_Args_Sign& other) const
		{
			return !operator==(other);
		}
	};

	// creates a new function arguments signature
	inline static Func_Args_Sign
	func_args_sign_new()
	{
		return Func_Args_Sign{};
	}

	// frees the given function arguments signature
	inline static void
	func_args_sign_free(Func_Args_Sign& self)
	{
		mn::buf_free(self.types);
	}

	inline static void
	destruct(Func_Args_Sign& self)
	{
		func_args_sign_free(self);
	}

	// used to hash a function arguments signature
	struct Func_Args_Sign_Hasher
	{
		inline size_t
		operator()(const Func_Args_Sign& args) const
		{
			return mn::murmur_hash(block_from(args.types));
		}
	};

	// describes a function signature
	struct Func_Sign
	{
		Func_Args_Sign args;
		Type* return_type;

		bool
		operator==(const Func_Sign& other) const
		{
			if (args != other.args)
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
		func_args_sign_free(self.args);
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
			auto args_hash = Func_Args_Sign_Hasher{}(sign.args);
			return mn::hash_mix(args_hash, mn::Hash<Type*>()(sign.return_type));
		}
	};

	// array signature, which consists of the underlying type alongside the count
	struct Array_Sign
	{
		Type* base;
		int64_t count;

		bool
		operator==(const Array_Sign& other) const
		{
			return base == other.base && count == other.count;
		}

		bool
		operator!=(const Array_Sign& other) const
		{
			return !operator==(other);
		}
	};

	// used to hash array signature
	struct Array_Sign_Hasher
	{
		inline size_t
		operator()(const Array_Sign& sign) const
		{
			return mn::hash_mix(mn::Hash<Type*>{}(sign.base), mn::Hash<int64_t>{}(sign.count));
		}
	};

	struct Struct_Field_Type
	{
		Tkn name;
		Type* type;
		Expr* default_value;
		size_t offset;
	};

	struct Enum_Field_Type
	{
		Tkn name;
		Expr_Value value;
	};

	struct Type_Overload_Entry
	{
		Location loc;
		Type* type;
	};

	enum TEXTURE_TYPE
	{
		TEXTURE_TYPE_1D,
		TEXTURE_TYPE_2D,
		TEXTURE_TYPE_3D,
		TEXTURE_TYPE_CUBE,
	};

	// represents a data type
	struct Type
	{
		enum KIND
		{
			KIND_INCOMPLETE,
			KIND_COMPLETING,

			KIND_VOID,
			KIND_BOOL,
			KIND_INT,
			KIND_UINT,
			KIND_FLOAT,
			KIND_DOUBLE,
			KIND_VEC,
			KIND_MAT,
			KIND_FUNC,
			KIND_STRUCT,
			KIND_TEXTURE,
			KIND_PACKAGE,
			KIND_FUNC_OVERLOAD_SET,
			KIND_ARRAY,
			KIND_ENUM,
			KIND_SAMPLER,
		};

		KIND kind;
		size_t size;
		size_t alignment;
		union
		{
			Func_Sign func;

			struct
			{
				Type* base;
				int width;
			} vec;

			struct
			{
				// matrices are of type float
				Type* base;
				int width;
			} mat;

			struct
			{
				Symbol* symbol;
				mn::Buf<Struct_Field_Type> fields;
				mn::Map<const char*, size_t> fields_by_name;
			} struct_type;

			struct
			{
				Unit_Package* package;
			} package_type;

			struct
			{
				Symbol* symbol;
				mn::Map<Func_Args_Sign, Type_Overload_Entry, Func_Args_Sign_Hasher> overloads;
			} func_overload_set_type;

			struct
			{
				TEXTURE_TYPE type;
			} texture;

			struct
			{
				Type* base;
				int64_t count;
			} array;

			struct
			{
				Symbol* symbol;
				mn::Buf<Enum_Field_Type> fields;
				mn::Map<const char*, size_t> fields_by_name;
			} enum_type;
		};
	};

	// language basic datatypes
	SABRE_EXPORT extern Type* type_void;
	SABRE_EXPORT extern Type* type_bool;
	SABRE_EXPORT extern Type* type_int;
	SABRE_EXPORT extern Type* type_lit_int;
	SABRE_EXPORT extern Type* type_uint;
	SABRE_EXPORT extern Type* type_float;
	SABRE_EXPORT extern Type* type_lit_float;
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
	SABRE_EXPORT extern Type* type_mat2;
	SABRE_EXPORT extern Type* type_mat3;
	SABRE_EXPORT extern Type* type_mat4;
	SABRE_EXPORT extern Type* type_texture1d;
	SABRE_EXPORT extern Type* type_texture2d;
	SABRE_EXPORT extern Type* type_texture3d;
	SABRE_EXPORT extern Type* type_texture_cube;
	SABRE_EXPORT extern Type* type_sampler;

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
		else if (str == "mat2")
			return type_mat2;
		else if (str == "mat3")
			return type_mat3;
		else if (str == "mat4")
			return type_mat4;
		else if (str == "Texture1D")
			return type_texture1d;
		else if (str == "Texture2D")
			return type_texture2d;
		else if (str == "Texture3D")
			return type_texture3d;
		else if (str == "TextureCube")
			return type_texture_cube;
		else if (str == "Sampler")
			return type_sampler;
		else
			return type_void;
	}

	// returns whether two types are equal
	inline static bool
	type_is_equal(Type* a, Type* b)
	{
		if ((a == type_lit_int && b == type_int) || (a == type_int && b == type_lit_int))
		{
			return true;
		}
		else if ((a == type_lit_float && b == type_float) || (a == type_float && b == type_lit_float))
		{
			return true;
		}
		else if ((a == type_float && b == type_lit_int) || (a == type_lit_int && b == type_float))
		{
			return true;
		}
		else if ((a == type_int && b == type_lit_float) || (a == type_lit_float && b == type_int))
		{
			return true;
		}
		else
		{
			return a == b;
		}
	}

	// returns whether a type can use ++v, --v
	inline static bool
	type_can_increment(Type* a)
	{
		return (
			type_is_equal(a, type_int) ||
			type_is_equal(a, type_uint) ||
			type_is_equal(a, type_float) ||
			type_is_equal(a, type_double)
		);
	}

	// returns whether a type can use +v, -v
	inline static bool
	type_can_negate(Type* a)
	{
		return (
			type_is_equal(a, type_int) ||
			type_is_equal(a, type_uint) ||
			type_is_equal(a, type_float) ||
			type_is_equal(a, type_double) ||
			(a->kind == Type::KIND_VEC && type_can_negate(a->vec.base)) ||
			(a->kind == Type::KIND_MAT && type_can_negate(a->mat.base))
		);
	}

	// returns whether a type is numerical scalar
	inline static bool
	type_is_numeric_scalar(Type* a)
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
		return (
			a->kind == Type::KIND_FUNC ||
			a->kind == Type::KIND_FUNC_OVERLOAD_SET
		);
	}

	// returns whether a type is a literal
	inline static bool
	type_is_literal(Type* a)
	{
		return (
			a == type_lit_int ||
			a == type_lit_float
		);
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

	// returns whether the type can be used in shader input
	inline static bool
	type_is_shader_input(Type* a)
	{
		return (
			a == type_int ||
			a == type_uint ||
			a == type_float ||
			a == type_double ||
			a == type_vec2 ||
			a == type_vec3 ||
			a == type_vec4 ||
			a == type_ivec2 ||
			a == type_ivec3 ||
			a == type_ivec4 ||
			a == type_uvec2 ||
			a == type_uvec3 ||
			a == type_uvec4 ||
			a == type_dvec2 ||
			a == type_dvec3 ||
			a == type_dvec4
		);
	}

	// returns whether the type can be used in uniform block
	inline static bool
	type_is_uniform(Type* a)
	{
		return (
			a == type_int ||
			a == type_uint ||
			a == type_float ||
			a == type_double ||
			a == type_vec2 ||
			a == type_vec3 ||
			a == type_vec4 ||
			a == type_ivec2 ||
			a == type_ivec3 ||
			a == type_ivec4 ||
			a == type_uvec2 ||
			a == type_uvec3 ||
			a == type_uvec4 ||
			a == type_dvec2 ||
			a == type_dvec3 ||
			a == type_dvec4 ||
			a == type_mat2 ||
			a == type_mat3 ||
			a == type_mat4
		);
	}

	// returns whether a type is a struct
	inline static bool
	type_is_struct(Type* t)
	{
		return t->kind == Type::KIND_STRUCT;
	}

	// returns whether the type is a vector
	inline static bool
	type_is_vec(Type* t)
	{
		return t->kind == Type::KIND_VEC;
	}

	// returns whether a type is an array
	inline static bool
	type_is_array(Type* t)
	{
		return t->kind == Type::KIND_ARRAY;
	}

	// returns whether the type is an unbounded array
	inline static bool
	type_is_unbounded_array(Type* t)
	{
		return type_is_array(t) && t->array.count == -1;
	}

	// returns whether the type is an bounded array
	inline static bool
	type_is_bounded_array(Type* t)
	{
		return type_is_array(t) && t->array.count > -1;
	}

	// returns whether the type is an enum
	inline static bool
	type_is_enum(Type* t)
	{
		return t->kind == Type::KIND_ENUM;
	}

	// returns whether the type can be used in a bit operation
	inline static bool
	type_has_bit_ops(Type* a)
	{
		return (
			type_is_equal(a, type_int) ||
			type_is_equal(a, type_uint) ||
			type_is_enum(a) || // all enum types has int values
			(type_is_vec(a) && type_has_bit_ops(a->vec.base))
		);
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

	inline static Type*
	type_field_by_index(Type* t, size_t index)
	{
		switch (t->kind)
		{
		case Type::KIND_VOID:
		case Type::KIND_BOOL:
		case Type::KIND_INT:
		case Type::KIND_UINT:
		case Type::KIND_FLOAT:
		case Type::KIND_DOUBLE:
		case Type::KIND_FUNC:
		case Type::KIND_TEXTURE:
		case Type::KIND_PACKAGE:
		case Type::KIND_FUNC_OVERLOAD_SET:
			return nullptr;
		case Type::KIND_VEC:
			if (index < t->vec.width)
				return t->vec.base;
			return nullptr;
		case Type::KIND_MAT:
			if (index < t->mat.width)
				return t->mat.base;
			return nullptr;
		case Type::KIND_STRUCT:
			if (index < t->struct_type.fields.count)
				return t->struct_type.fields[index].type;
			return nullptr;
		case Type::KIND_ARRAY:
			if (index < t->array.count)
				return t->array.base;
			return nullptr;
		case Type::KIND_ENUM:
			if (index < t->enum_type.fields.count)
				return type_int;
			return nullptr;
		default:
			assert(false && "unreachable");
			return nullptr;
		}
	}

	inline static size_t
	type_fields_count(Type* t)
	{
		switch (t->kind)
		{
		case Type::KIND_VOID:
		case Type::KIND_BOOL:
		case Type::KIND_INT:
		case Type::KIND_UINT:
		case Type::KIND_FLOAT:
		case Type::KIND_DOUBLE:
		case Type::KIND_FUNC:
		case Type::KIND_TEXTURE:
		case Type::KIND_PACKAGE:
		case Type::KIND_FUNC_OVERLOAD_SET:
			return 0;
		case Type::KIND_VEC:
			return t->vec.width;
		case Type::KIND_MAT:
			return t->mat.width;
		case Type::KIND_STRUCT:
			return t->struct_type.fields.count;
		case Type::KIND_ARRAY:
			return t->array.count;
		case Type::KIND_ENUM:
			return t->enum_type.fields.count;
		default:
			assert(false && "unreachable");
			return 0;
		}
	}

	// returns the type lane width
	inline static size_t
	type_width(Type* t)
	{
		switch (t->kind)
		{
		case Type::KIND_VOID:
		case Type::KIND_FUNC:
		case Type::KIND_TEXTURE:
		case Type::KIND_PACKAGE:
		case Type::KIND_FUNC_OVERLOAD_SET:
		case Type::KIND_STRUCT:
			return 0;
		case Type::KIND_BOOL:
		case Type::KIND_INT:
		case Type::KIND_UINT:
		case Type::KIND_FLOAT:
		case Type::KIND_DOUBLE:
		case Type::KIND_ENUM:
			return 1;
		case Type::KIND_VEC:
			return t->vec.width;
		case Type::KIND_MAT:
			return t->mat.width;
		case Type::KIND_ARRAY:
			return t->array.count;
		default:
			assert(false && "unreachable");
			return 0;
		}
	}

	// interns the different types to make comparisons and memory management easier
	struct Type_Interner
	{
		mn::memory::Arena* arena;
		mn::Map<Func_Sign, Type*, Func_Sign_Hasher> func_table;
		mn::Map<Unit_Package*, Type*> package_table;
		mn::Map<Array_Sign, Type*, Array_Sign_Hasher> array_table;
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

	// creates a new incomplete type for the given symbol
	SABRE_EXPORT Type*
	type_interner_incomplete(Type_Interner& self, Symbol* symbol);

	// completes the given struct/aggregate types
	SABRE_EXPORT void
	type_interner_complete_struct(Type_Interner& self, Type* type, mn::Buf<Struct_Field_Type> fields, mn::Map<const char*, size_t> fields_table);

	// completes an enum type
	SABRE_EXPORT void
	type_interner_complete_enum(Type_Interner& self, Type* type, mn::Buf<Enum_Field_Type> fields, mn::Map<const char*, size_t> fields_table);

	// creates a new package type
	SABRE_EXPORT Type*
	type_interner_package(Type_Interner& self, Unit_Package* package);

	// creates a new overload set type
	SABRE_EXPORT Type*
	type_interner_overload_set(Type_Interner& self, Symbol* symbol);

	// creates a new array type
	SABRE_EXPORT Type*
	type_interner_array(Type_Interner& self, Array_Sign sign);

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
		};

		KIND kind;
		STATE state;
		Type* type;
		Unit_Package* package;
		Scope* scope;
		const char* name;
		const char* package_name;

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
		};
	};

	// creates a new symbol for a constant declaration
	inline static Symbol*
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

	// creates a new symbol for a variable declaration
	inline static Symbol*
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

	// creates a new symbol for a function declaration
	inline static Symbol*
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

	// creates a new symbol for a struct declaration
	inline static Symbol*
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

	// creates a new symbol for enum declaration
	inline static Symbol*
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

	// creates a new symbol for package declaration
	inline static Symbol*
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

	// creates a new symbol for function overload set given a function
	inline static Symbol*
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
		case Symbol::KIND_STRUCT:
			return self->struct_sym.decl->loc;
		case Symbol::KIND_PACKAGE:
			return self->package_sym.decl->loc;
		case Symbol::KIND_ENUM:
			return self->enum_sym.decl->loc;
		default:
			assert(false && "unreachable");
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
		default:
			assert(false && "unreachable");
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
			else if (t == sabre::type_int || t == sabre::type_lit_int)
			{
				return format_to(ctx.out(), "int");
			}
			else if (t == sabre::type_uint)
			{
				return format_to(ctx.out(), "uint");
			}
			else if (t == sabre::type_float || t == sabre::type_lit_float)
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
			else if (t == sabre::type_mat2)
			{
				return format_to(ctx.out(), "mat2");
			}
			else if (t == sabre::type_mat3)
			{
				return format_to(ctx.out(), "mat3");
			}
			else if (t == sabre::type_mat4)
			{
				return format_to(ctx.out(), "mat4");
			}
			else if (t->kind == sabre::Type::KIND_FUNC)
			{
				format_to(ctx.out(), "func(");
				for (size_t i = 0; i < t->func.args.types.count; ++i)
				{
					if (i > 0)
						format_to(ctx.out(), ", ");
					format_to(ctx.out(), ":{}", t->func.args.types[i]);
				}
				format_to(ctx.out(), "):{}", t->func.return_type);
				return ctx.out();
			}
			else if (t->kind == sabre::Type::KIND_STRUCT)
			{
				return format_to(ctx.out(), "struct {}", t->struct_type.symbol->name);
			}
			else if (t->kind == sabre::Type::KIND_PACKAGE)
			{
				return format_to(ctx.out(), "package '{}'", t->package_type.package->absolute_path);
			}
			else if (t->kind == sabre::Type::KIND_FUNC_OVERLOAD_SET)
			{
				size_t overload_i = 0;
				for (auto [_, overload]: t->func_overload_set_type.overloads)
				{
					if (overload_i > 0)
						format_to(ctx.out(), "\n");
					format_to(ctx.out(), "{}. func(", overload_i++);
					for (size_t i = 0; i < overload.type->func.args.types.count; ++i)
					{
						if (i > 0)
							format_to(ctx.out(), ", ");
						format_to(ctx.out(), ":{}", overload.type->func.args.types[i]);
					}
					format_to(ctx.out(), "):{}", overload.type->func.return_type);
				}
				return ctx.out();
			}
			else if (t->kind == sabre::Type::KIND_TEXTURE)
			{
				switch (t->texture.type)
				{
				case sabre::TEXTURE_TYPE_1D:
					return format_to(ctx.out(), "Texture1D");
				case sabre::TEXTURE_TYPE_2D:
					return format_to(ctx.out(), "Texture2D");
				case sabre::TEXTURE_TYPE_3D:
					return format_to(ctx.out(), "Texture3D");
				case sabre::TEXTURE_TYPE_CUBE:
					return format_to(ctx.out(), "TextureCube");
				default:
					assert(false && "unreachable");
					return format_to(ctx.out(), "<UNKNOWN TYPE>");
				}
			}
			else if (t->kind == sabre::Type::KIND_ARRAY)
			{
				if (t->array.count == -1)
					return format_to(ctx.out(), "[]{}", t->array.base);
				else
					return format_to(ctx.out(), "[{}]{}", t->array.count, t->array.base);
			}
			else if (t->kind == sabre::Type::KIND_ENUM)
			{
				return format_to(ctx.out(), "enum {}", t->enum_type.symbol->name);
			}
			else if (t->kind == sabre::Type::KIND_SAMPLER)
			{
				return format_to(ctx.out(), "sampler");
			}
			else
			{
				assert(false && "unreachable");
				return format_to(ctx.out(), "<UNKNOWN TYPE>");
			}
		}
	};
}