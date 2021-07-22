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

	inline static Type
	_mat_builtin(Type* base, size_t width)
	{
		Type self{};
		self.kind = Type::KIND_MAT;
		self.mat.base = base;
		self.mat.width = width;
		return self;
	}

	inline static Type
	_texture_builtin(TEXTURE_TYPE type)
	{
		Type self{};
		self.kind = Type::KIND_TEXTURE;
		self.texture.type = type;
		return self;
	}

	static Type _type_void { Type::KIND_VOID };
	static Type _type_bool { Type::KIND_BOOL };
	static Type _type_int { Type::KIND_INT };
	static Type _type_lit_int { Type::KIND_INT };
	static Type _type_uint { Type::KIND_UINT };
	static Type _type_float { Type::KIND_FLOAT };
	static Type _type_lit_float { Type::KIND_FLOAT };
	static Type _type_double { Type::KIND_DOUBLE };
	static Type _type_vec2 = _vec_builtin(type_float, 2);
	static Type _type_vec3 = _vec_builtin(type_float, 3);
	static Type _type_vec4 = _vec_builtin(type_float, 4);
	static Type _type_bvec2 = _vec_builtin(type_bool, 2);
	static Type _type_bvec3 = _vec_builtin(type_bool, 3);
	static Type _type_bvec4 = _vec_builtin(type_bool, 4);
	static Type _type_ivec2 = _vec_builtin(type_int, 2);
	static Type _type_ivec3 = _vec_builtin(type_int, 3);
	static Type _type_ivec4 = _vec_builtin(type_int, 4);
	static Type _type_uvec2 = _vec_builtin(type_uint, 2);
	static Type _type_uvec3 = _vec_builtin(type_uint, 3);
	static Type _type_uvec4 = _vec_builtin(type_uint, 4);
	static Type _type_dvec2 = _vec_builtin(type_double, 2);
	static Type _type_dvec3 = _vec_builtin(type_double, 3);
	static Type _type_dvec4 = _vec_builtin(type_double, 4);
	static Type _type_mat2 = _mat_builtin(type_float, 2);
	static Type _type_mat3 = _mat_builtin(type_float, 3);
	static Type _type_mat4 = _mat_builtin(type_float, 4);
	static Type _type_texture1d = _texture_builtin(TEXTURE_TYPE_1D);
	static Type _type_texture2d = _texture_builtin(TEXTURE_TYPE_2D);
	static Type _type_texture3d = _texture_builtin(TEXTURE_TYPE_3D);
	static Type _type_texture_cube = _texture_builtin(TEXTURE_TYPE_CUBE);

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
		mn::map_free(self->generated_names);
		mn::free(self);
	}

	Type* type_void = &_type_void;
	Type* type_bool = &_type_bool;
	Type* type_int = &_type_int;
	Type* type_lit_int = &_type_lit_int;
	Type* type_lit_float = &_type_lit_float;
	Type* type_uint = &_type_uint;
	Type* type_float = &_type_float;
	Type* type_double = &_type_double;
	Type* type_vec2 = &_type_vec2;
	Type* type_vec3 = &_type_vec3;
	Type* type_vec4 = &_type_vec4;
	Type* type_bvec2 = &_type_bvec2;
	Type* type_bvec3 = &_type_bvec3;
	Type* type_bvec4 = &_type_bvec4;
	Type* type_ivec2 = &_type_ivec2;
	Type* type_ivec3 = &_type_ivec3;
	Type* type_ivec4 = &_type_ivec4;
	Type* type_uvec2 = &_type_uvec2;
	Type* type_uvec3 = &_type_uvec3;
	Type* type_uvec4 = &_type_uvec4;
	Type* type_dvec2 = &_type_dvec2;
	Type* type_dvec3 = &_type_dvec3;
	Type* type_dvec4 = &_type_dvec4;
	Type* type_mat2 = &_type_mat2;
	Type* type_mat3 = &_type_mat3;
	Type* type_mat4 = &_type_mat4;
	Type* type_texture1d = &_type_texture1d;
	Type* type_texture2d = &_type_texture2d;
	Type* type_texture3d = &_type_texture3d;
	Type* type_texture_cube = &_type_texture_cube;

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
		mn::map_free(self.package_table);
		mn::map_free(self.array_table);
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

	Type*
	type_interner_incomplete(Type_Interner& self, Symbol* symbol)
	{
		auto new_type = mn::alloc_zerod_from<Type>(self.arena);
		new_type->kind = Type::KIND_INCOMPLETE;
		new_type->struct_type.symbol = symbol;
		return new_type;
	}

	void
	type_interner_complete(Type_Interner& self, Type* type, mn::Buf<Field_Type> fields, mn::Map<const char*, size_t> fields_table)
	{
		assert(type->kind == Type::KIND_COMPLETING);
		type->kind = Type::KIND_STRUCT;
		type->struct_type.fields = fields;
		type->struct_type.fields_by_name = fields_table;
	}

	Type*
	type_interner_package(Type_Interner& self, Unit_Package* package)
	{
		if (auto it = mn::map_lookup(self.package_table, package))
			return it->value;
		auto new_type = mn::alloc_zerod_from<Type>(self.arena);
		new_type->kind = Type::KIND_PACKAGE;
		new_type->package_type.package = package;
		mn::map_insert(self.package_table, package, new_type);
		return new_type;
	}

	Type*
	type_interner_overload_set(Type_Interner& self, Symbol* symbol)
	{
		auto new_type = mn::alloc_zerod_from<Type>(self.arena);
		new_type->kind = Type::KIND_FUNC_OVERLOAD_SET;
		new_type->func_overload_set_type.symbol = symbol;
		new_type->func_overload_set_type.overloads = mn::map_with_allocator<Func_Args_Sign, Type_Overload_Entry, Func_Args_Sign_Hasher>(self.arena);
		return new_type;
	}

	Type*
	type_interner_array(Type_Interner& self, Array_Sign sign)
	{
		if (auto it = mn::map_lookup(self.array_table, sign))
			return it->value;
		auto new_type = mn::alloc_zerod_from<Type>(self.arena);
		new_type->kind = Type::KIND_ARRAY;
		new_type->array.base = sign.base;
		new_type->array.count = sign.count;
		mn::map_insert(self.array_table, sign, new_type);
		return new_type;
	}
}