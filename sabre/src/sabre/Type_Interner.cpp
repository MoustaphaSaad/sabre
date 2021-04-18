#include "sabre/Type_Interner.h"

namespace sabre
{
	static Type _type_void { Type::KIND_VOID };
	static Type _type_bool { Type::KIND_BOOL };
	static Type _type_int { Type::KIND_INT };
	static Type _type_uint { Type::KIND_UINT };
	static Type _type_float32 { Type::KIND_FLOAT32 };
	static Type _type_float64 { Type::KIND_FLOAT64 };

	// API
	Scope*
	scope_new(Scope* parent, const char* name)
	{
		auto self = mn::alloc_zerod<Scope>();
		self->parent = parent;
		self->name = name;
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
	Type* type_float32 = &_type_float32;
	Type* type_float64 = &_type_float64;

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