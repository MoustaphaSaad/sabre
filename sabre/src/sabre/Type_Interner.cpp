#include "sabre/Type_Interner.h"

namespace sabre
{
	inline static Type
	_vec_builtin(Type* base, size_t width)
	{
		Type self{};
		self.kind = Type::KIND_VEC;
		self.vec.base = base;
		self.vec.width = width;
		return self;
	}

	static Type _type_void { Type::KIND_VOID };
	static Type _type_bool { Type::KIND_BOOL };
	static Type _type_int { Type::KIND_INT };
	static Type _type_uint { Type::KIND_UINT };
	static Type _type_float { Type::KIND_FLOAT };
	static Type _type_double { Type::KIND_DOUBLE };
	static Type _type_vec2 = _vec_builtin(type_float, 2);
	static Type _type_vec3 = _vec_builtin(type_float, 3);
	static Type _type_vec4 = _vec_builtin(type_float, 4);

	// API
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
		mn::buf_free(self->symbols);
		mn::map_free(self->symbol_table);
		mn::free(self);
	}

	Type* type_void = &_type_void;
	Type* type_bool = &_type_bool;
	Type* type_int = &_type_int;
	Type* type_uint = &_type_uint;
	Type* type_float = &_type_float;
	Type* type_double = &_type_double;
	Type* type_vec2 = &_type_vec2;
	Type* type_vec3 = &_type_vec3;
	Type* type_vec4 = &_type_vec4;

	Type_Interner
	type_interner_new()
	{
		Type_Interner self{};
		self.arena = mn::allocator_arena_new();
		return self;
	}

	void
	type_interner_free(Type_Interner& self)
	{
		mn::allocator_free(self.arena);
		destruct(self.func_table);
	}

	Type*
	type_interner_func(Type_Interner& self, Func_Sign sign)
	{
		if(auto it = mn::map_lookup(self.func_table, sign))
		{
			func_sign_free(sign);
			return it->value;
		}

		auto new_type = mn::alloc_zerod_from<Type>(self.arena);
		new_type->kind = Type::KIND_FUNC;
		new_type->func = sign;
		mn::map_insert(self.func_table, sign, new_type);
		return new_type;
	}
}