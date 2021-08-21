#include "sabre/Type_Interner.h"

namespace sabre
{
	inline static Type
	_vec_builtin(Type* base, size_t width, size_t size, size_t alignment)
	{
		Type self{};
		self.kind = Type::KIND_VEC;
		self.size = size;
		self.alignment = alignment;
		self.vec.base = base;
		self.vec.width = width;
		return self;
	}

	inline static Type
	_mat_builtin(Type* base, size_t width, size_t size, size_t alignment)
	{
		Type self{};
		self.kind = Type::KIND_MAT;
		self.size = size;
		self.alignment = alignment;
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

	static Type _type_void { Type::KIND_VOID, 0, 0 };
	static Type _type_bool { Type::KIND_BOOL, 1, 4 };
	static Type _type_int { Type::KIND_INT, 4, 4 };
	static Type _type_lit_int { Type::KIND_INT, 4, 4 };
	static Type _type_uint { Type::KIND_UINT, 4, 4 };
	static Type _type_float { Type::KIND_FLOAT, 4, 4 };
	static Type _type_lit_float { Type::KIND_FLOAT, 4, 4 };
	static Type _type_double { Type::KIND_DOUBLE, 8, 8 };
	static Type _type_vec2 = _vec_builtin(type_float, 2, 8, 8);
	static Type _type_vec3 = _vec_builtin(type_float, 3, 12, 16);
	static Type _type_vec4 = _vec_builtin(type_float, 4, 16, 16);
	static Type _type_bvec2 = _vec_builtin(type_bool, 2, 2, 2);
	static Type _type_bvec3 = _vec_builtin(type_bool, 3, 3, 4);
	static Type _type_bvec4 = _vec_builtin(type_bool, 4, 4, 4);
	static Type _type_ivec2 = _vec_builtin(type_int, 2, 8, 8);
	static Type _type_ivec3 = _vec_builtin(type_int, 3, 12, 16);
	static Type _type_ivec4 = _vec_builtin(type_int, 4, 16, 16);
	static Type _type_uvec2 = _vec_builtin(type_uint, 2, 8, 8);
	static Type _type_uvec3 = _vec_builtin(type_uint, 3, 12, 16);
	static Type _type_uvec4 = _vec_builtin(type_uint, 4, 16, 16);
	static Type _type_dvec2 = _vec_builtin(type_double, 2, 16, 16);
	static Type _type_dvec3 = _vec_builtin(type_double, 3, 24, 32);
	static Type _type_dvec4 = _vec_builtin(type_double, 4, 32, 32);
	static Type _type_mat2 = _mat_builtin(type_float, 2, 32, 16);
	static Type _type_mat3 = _mat_builtin(type_float, 3, 48, 16);
	static Type _type_mat4 = _mat_builtin(type_float, 4, 64, 16);
	static Type _type_texture1d = _texture_builtin(TEXTURE_TYPE_1D);
	static Type _type_texture2d = _texture_builtin(TEXTURE_TYPE_2D);
	static Type _type_texture3d = _texture_builtin(TEXTURE_TYPE_3D);
	static Type _type_texture_cube = _texture_builtin(TEXTURE_TYPE_CUBE);
	static Type _type_sampler { Type::KIND_SAMPLER, 0, 0 };

	inline static size_t
	_round_up(size_t num, size_t factor)
	{
		return num + factor - 1 - (num + factor - 1) % factor;
	}

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
		if (self)
		{
			mn::buf_free(self->symbols);
			mn::map_free(self->symbol_table);
			mn::map_free(self->generated_names);
			mn::free(self);
		}
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
	Type* type_sampler = &_type_sampler;

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
	type_interner_complete_struct(Type_Interner& self, Type* type, mn::Buf<Struct_Field_Type> fields, mn::Map<const char*, size_t> fields_table)
	{
		assert(type->kind == Type::KIND_COMPLETING);
		type->kind = Type::KIND_STRUCT;
		type->struct_type.fields = fields;
		type->struct_type.fields_by_name = fields_table;

		type->alignment = 1;
		type->size = 0;
		for (auto& field: fields)
		{
			if (field.type->alignment > type->alignment)
				type->alignment = field.type->alignment;
			if (field.type->alignment > 0)
				if (type->size % field.type->alignment != 0)
					type->size = _round_up(type->size, type_vec4->size);
			field.offset = type->size;
			type->size += field.type->size;
		}
		type->alignment = _round_up(type->alignment, type_vec4->size);
		type->size = _round_up(type->size, type->alignment);
	}

	void
	type_interner_complete_enum(Type_Interner& self, Type* type, mn::Buf<Enum_Field_Type> fields, mn::Map<const char*, size_t> fields_table)
	{
		assert(type->kind == Type::KIND_COMPLETING);
		type->kind = Type::KIND_ENUM;
		type->enum_type.fields = fields;
		type->enum_type.fields_by_name = fields_table;
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
		new_type->alignment = _round_up(sign.base->size, type_vec4->size);
		if (sign.count >= 0)
			new_type->size = _round_up(sign.base->size, type_vec4->size) * sign.count;
		new_type->array.base = sign.base;
		new_type->array.count = sign.count;
		mn::map_insert(self.array_table, sign, new_type);
		return new_type;
	}
}