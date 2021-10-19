#include "sabre/Type_Interner.h"

#include <mn/Assert.h>

namespace sabre
{
	inline static Type
	_vec_builtin(Type* base, size_t width, size_t size, size_t alignment)
	{
		Type self{};
		self.kind = Type::KIND_VEC;
		self.unaligned_size = size;
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
		self.unaligned_size = size;
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
	static Type _type_sampler {Type::KIND_SAMPLER, 0, 0};

	inline static void
	_calc_struct_size(Type* type)
	{
		type->alignment = 1;
		type->unaligned_size = 0;
		for (auto& field: type->struct_type.fields)
		{
			if (field.type->alignment > type->alignment)
				type->alignment = field.type->alignment;
			if (field.type->alignment > 0)
				if (type->unaligned_size % field.type->alignment != 0)
					type->unaligned_size = _round_up(type->unaligned_size, field.type->alignment);
			field.offset = type->unaligned_size;
			type->unaligned_size += field.type->unaligned_size;
		}
		type->alignment = _round_up(type->alignment, type_vec4->alignment);
		// type->unaligned_size = _round_up(type->size, type_vec4->size);
	}

	inline static Template_Instantiation_Sign
	_generate_template_instantiation_sign(Type* base_type, const mn::Buf<Type*>& args)
	{
		if (base_type->template_base_type == nullptr)
		{
			Template_Instantiation_Sign sign{};
			sign.template_type = base_type;
			sign.args = mn::buf_memcpy_clone(args);
			return sign;
		}

		auto base_args = mn::buf_memcpy_clone<Type*>(base_type->template_base_args);
		for (size_t i = 0; i < args.count; ++i)
			base_args[base_type->template_args_index[i]] = args[i];
		auto sign = _generate_template_instantiation_sign(base_type->template_base_type, base_args);
		mn::buf_free(base_args);

		return sign;
	}

	// API
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

	Type_Interner*
	type_interner_new()
	{
		auto self = mn::alloc_zerod<Type_Interner>();
		self->arena = mn::allocator_arena_new();
		return self;
	}

	void
	type_interner_free(Type_Interner* self)
	{
		if (self)
		{
			mn::allocator_free(self->arena);
			destruct(self->template_func_sign_list);
			destruct(self->func_table);
			mn::map_free(self->package_table);
			mn::map_free(self->array_table);
			mn::map_free(self->typename_table);
			destruct(self->instantiation_table);
			destruct(self->func_instantiation_decls);
			mn::free(self);
		}
	}

	Type*
	type_interner_func(Type_Interner* self, Func_Sign sign, Decl* decl, mn::Buf<Type*> template_args)
	{
		// template functions doesn't get to be interned yet, they do when you instantiate them
		if (template_args.count > 0)
		{
			auto new_type = mn::alloc_zerod_from<Type>(self->arena);
			new_type->kind = Type::KIND_FUNC;
			new_type->as_func.sign = sign;
			new_type->template_args = template_args;
			new_type->as_func.template_func_decl = decl;

			new_type->template_args_index = mn::buf_with_allocator<size_t>(self->arena);
			for (size_t i = 0; i < new_type->template_args.count; ++i)
				mn::buf_push(new_type->template_args_index, i);

			mn::buf_push(self->template_func_sign_list, sign);
			return new_type;
		}

		// this is the code path for normal functions (without template arguments)
		if(auto it = mn::map_lookup(self->func_table, sign))
		{
			func_sign_free(sign);
			return it->value;
		}

		auto new_type = mn::alloc_zerod_from<Type>(self->arena);
		new_type->kind = Type::KIND_FUNC;
		new_type->as_func.sign = sign;
		mn::map_insert(self->func_table, sign, new_type);
		return new_type;
	}

	Type*
	type_interner_template_func_instantiate(Type_Interner* self, Type* func_type, const mn::Buf<Type*>& template_args_types, const mn::Buf<Type*>& args_types, Type* return_type)
	{
		Template_Instantiation_Sign sign{};
		sign.template_type = func_type;
		sign.args = template_args_types;
		if (auto it = mn::map_lookup(self->instantiation_table, sign))
			return it->value;

		auto func_sign = func_sign_new();
		mn::buf_concat(func_sign.args.types, args_types);
		func_sign.return_type = return_type;
		auto new_type = type_interner_func(self, func_sign, nullptr, {});

		sign.args = mn::buf_memcpy_clone(template_args_types);
		mn::map_insert(self->instantiation_table, sign, new_type);

		return new_type;
	}

	Type*
	type_interner_incomplete(Type_Interner* self, Symbol* symbol)
	{
		auto new_type = mn::alloc_zerod_from<Type>(self->arena);
		new_type->kind = Type::KIND_INCOMPLETE;
		new_type->struct_type.symbol = symbol;
		return new_type;
	}

	void
	type_interner_complete_struct(Type_Interner* self, Type* type, mn::Buf<Struct_Field_Type> fields, mn::Map<const char*, size_t> fields_table, mn::Buf<Type*> template_args)
	{
		mn_assert(type->kind == Type::KIND_COMPLETING);
		type->kind = Type::KIND_STRUCT;
		type->template_args = template_args;
		type->struct_type.fields = fields;
		type->struct_type.fields_by_name = fields_table;
		_calc_struct_size(type);

		type->template_args_index = mn::buf_with_allocator<size_t>(self->arena);
		for (size_t i = 0; i < type->template_args.count; ++i)
			mn::buf_push(type->template_args_index, i);

		// if (type_is_templated(type))
		// {
		// 	Template_Instantiation_Sign sign{};
		// 	sign.template_type = type;
		// 	sign.args = type->template_args;
		// 	mn::map_insert(self->instantiation_table, sign, type);
		// }
	}

	Type*
	type_interner_template_instantiate(Type_Interner* self, Type* base_type, const mn::Buf<Type*>& args, Decl* decl)
	{
		auto sign = _generate_template_instantiation_sign(base_type, args);

		if (auto it = mn::map_lookup(self->instantiation_table, sign))
		{
			mn::buf_free(sign.args);
			return it->value;
		}

		mn_assert(base_type->template_args.count == args.count);

		switch (base_type->kind)
		{
		case Type::KIND_STRUCT:
		{
			auto new_type = mn::alloc_zerod_from<Type>(self->arena);
			new_type->kind = Type::KIND_STRUCT;

			new_type->template_args = mn::buf_with_allocator<Type*>(self->arena);
			new_type->template_args_index = mn::buf_with_allocator<size_t>(self->arena);
			mn::buf_reserve(new_type->template_args, args.count);
			for (size_t i = 0; i < base_type->template_args.count; ++i)
			{
				if (type_is_typename(args[i]))
				{
					mn::buf_push(new_type->template_args, args[i]);
					mn::buf_push(new_type->template_args_index, i);
				}
			}

			new_type->struct_type.symbol = base_type->struct_type.symbol;
			new_type->struct_type.fields = mn::buf_memcpy_clone(base_type->struct_type.fields, self->arena);
			new_type->struct_type.fields_by_name = mn::map_memcpy_clone(base_type->struct_type.fields_by_name, self->arena);

			for (auto& field: new_type->struct_type.fields)
			{
				if (type_is_typename(field.type))
				{
					size_t index = base_type->template_args.count;
					for (size_t i = 0; i < base_type->template_args.count; ++i)
					{
						if (base_type->template_args[i] == field.type)
						{
							index = i;
							break;
						}
					}
					field.type = args[index];
				}
				else if (type_is_templated(field.type))
				{
					// we should provide the template type here
					auto field_args = mn::buf_clone(field.type->template_args, mn::memory::tmp());
					for (auto& field_arg: field_args)
					{
						for (size_t i = 0; i < base_type->template_args.count; ++i)
						{
							const auto& base_arg = base_type->template_args[i];
							if (field_arg == base_arg)
							{
								field_arg = args[i];
								break;
							}
						}
					}
					field.type = type_interner_template_instantiate(self, field.type, field_args, nullptr);
				}
			}

			_calc_struct_size(new_type);
			// sign.args = mn::buf_memcpy_clone(args);
			new_type->template_base_type = base_type;
			new_type->template_base_args = sign.args;
			mn::map_insert(self->instantiation_table, sign, new_type);
			return new_type;
		}
		case Type::KIND_FUNC:
		{
			auto func_sign = func_sign_new();
			for (auto arg_type: base_type->as_func.sign.args.types)
			{
				if (type_is_typename(arg_type))
				{
					size_t index = base_type->template_args.count;
					for (size_t i = 0; i < base_type->template_args.count; ++i)
					{
						if (base_type->template_args[i] == arg_type)
						{
							index = i;
							break;
						}
					}
					mn::buf_push(func_sign.args.types, args[index]);
				}
				else if (type_is_templated(arg_type))
				{
					auto instantiation_args = mn::buf_clone(arg_type->template_args, mn::memory::tmp());
					for (auto& arg: instantiation_args)
					{
						for (size_t i = 0; i < base_type->template_args.count; ++i)
						{
							const auto& base_arg = base_type->template_args[i];
							if (arg == base_arg)
							{
								arg = args[i];
								break;
							}
						}
					}
					auto resolved_arg_type = type_interner_template_instantiate(self, arg_type, instantiation_args, nullptr);
					mn::buf_push(func_sign.args.types, resolved_arg_type);
				}
			}

			// handle return type
			auto base_return_type = base_type->as_func.sign.return_type;
			if (type_is_typename(base_return_type))
			{
				size_t index = base_type->template_args.count;
				for (size_t i = 0; i < base_type->template_args.count; ++i)
				{
					if (base_type->template_args[i] == base_return_type)
					{
						index = i;
						break;
					}
				}
				func_sign.return_type = args[index];
			}
			else if (type_is_templated(base_return_type))
			{
				auto instantiation_args = mn::buf_clone(base_return_type->template_args, mn::memory::tmp());
				for (auto& arg: instantiation_args)
				{
					for (size_t i = 0; i < base_type->template_args.count; ++i)
					{
						const auto& base_arg = base_type->template_args[i];
						if (arg == base_arg)
						{
							arg = args[i];
							break;
						}
					}
				}
				auto resolved_arg_type = type_interner_template_instantiate(self, base_return_type, instantiation_args, nullptr);
				func_sign.return_type = resolved_arg_type;
			}
			else
			{
				func_sign.return_type = base_return_type;
			}

			auto template_args = mn::buf_with_allocator<Type*>(self->arena);
			mn::buf_reserve(template_args, args.count);
			for (size_t i = 0; i < base_type->template_args.count; ++i)
				if (type_is_typename(args[i]))
					mn::buf_push(template_args, args[i]);

			auto new_type = type_interner_func(self, func_sign, decl, template_args);
			new_type->template_args_index = mn::buf_with_allocator<size_t>(self->arena);
			mn::buf_reserve(template_args, args.count);
			for (size_t i = 0; i < base_type->template_args.count; ++i)
				if (type_is_typename(args[i]))
					mn::buf_push(new_type->template_args_index, args[i]);

			// sign.args = mn::buf_memcpy_clone(args);
			new_type->template_base_type = base_type;
			new_type->template_base_args = sign.args;
			mn::map_insert(self->instantiation_table, sign, new_type);
			return new_type;
		}
		default:
			mn_unreachable();
			return nullptr;
		}
	}

	void
	type_interner_complete_enum(Type_Interner* self, Type* type, mn::Buf<Enum_Field_Type> fields, mn::Map<const char*, size_t> fields_table)
	{
		mn_assert(type->kind == Type::KIND_COMPLETING);
		type->kind = Type::KIND_ENUM;
		type->enum_type.fields = fields;
		type->enum_type.fields_by_name = fields_table;
	}

	Type*
	type_interner_package(Type_Interner* self, Unit_Package* package)
	{
		if (auto it = mn::map_lookup(self->package_table, package))
			return it->value;
		auto new_type = mn::alloc_zerod_from<Type>(self->arena);
		new_type->kind = Type::KIND_PACKAGE;
		new_type->package_type.package = package;
		mn::map_insert(self->package_table, package, new_type);
		return new_type;
	}

	Type*
	type_interner_overload_set(Type_Interner* self, Symbol* symbol)
	{
		auto new_type = mn::alloc_zerod_from<Type>(self->arena);
		new_type->kind = Type::KIND_FUNC_OVERLOAD_SET;
		new_type->func_overload_set_type.symbol = symbol;
		new_type->func_overload_set_type.overloads = mn::map_with_allocator<Func_Args_Sign, Decl*, Func_Args_Sign_Hasher>(self->arena);
		return new_type;
	}

	Type*
	type_interner_array(Type_Interner* self, Array_Sign sign)
	{
		if (auto it = mn::map_lookup(self->array_table, sign))
			return it->value;

		auto new_type = mn::alloc_zerod_from<Type>(self->arena);
		new_type->kind = Type::KIND_ARRAY;
		new_type->alignment = _round_up(_type_aligned_size(sign.base), type_vec4->alignment);
		if (sign.count >= 0)
			new_type->unaligned_size = _type_aligned_size(sign.base) * sign.count;
		new_type->array.base = sign.base;
		new_type->array.count = sign.count;
		mn::map_insert(self->array_table, sign, new_type);
		return new_type;
	}

	Type*
	type_interner_typename(Type_Interner* self, Symbol* symbol)
	{
		if (auto type_it = mn::map_lookup(self->typename_table, symbol))
			return type_it->value;

		auto new_type = mn::alloc_zerod_from<Type>(self->arena);
		new_type->kind = Type::KIND_TYPENAME;
		new_type->typename_type.symbol = symbol;
		mn::map_insert(self->typename_table, symbol, new_type);
		return new_type;
	}

	void
	type_interner_add_func_instantiation_decl(Type_Interner* self, Type* base_type, const mn::Buf<Type*>& args, Decl* decl)
	{
		auto sign = _generate_template_instantiation_sign(base_type, args);
		if (auto it = mn::map_lookup(self->func_instantiation_decls, sign))
		{
			mn::buf_free(sign.args);
			mn_assert(it->value == decl);
		}
		mn::map_insert(self->func_instantiation_decls, sign, decl);
	}

	Decl*
	type_interner_find_func_instantiation_decl(Type_Interner* self, Type* base_type, const mn::Buf<Type*>& args)
	{
		auto sign = _generate_template_instantiation_sign(base_type, args);
		if (auto it = mn::map_lookup(self->func_instantiation_decls, sign))
		{
			mn::buf_free(sign.args);
			return it->value;
		}
		mn::buf_free(sign.args);
		return nullptr;
	}
}