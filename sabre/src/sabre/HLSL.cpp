#include "sabre/HLSL.h"
#include "sabre/AST.h"
#include "sabre/Type_Interner.h"
#include "sabre/Unit.h"

#include <mn/Assert.h>
#include <mn/Defer.h>

namespace sabre
{
	inline static const char* HLSL_KEYWORDS[] = {
		"AppendStructuredBuffer",
		"asm",
		"asm_fragment",
		"BlendState",
		"bool",
		"break",
		"Buffer",
		"ByteAddressBuffer",
		"case",
		"cbuffer",
		"centroid",
		"class",
		"column_major",
		"compile",
		"compile_fragment",
		"CompileShader",
		"const",
		"continue",
		"ComputeShader",
		"ConsumeStructuredBuffer",
		"default",
		"DepthStencilState",
		"DepthStencilView",
		"discard",
		"do",
		"double",
		"DomainShader",
		"dword",
		"else",
		"export",
		"extern",
		"false",
		"float",
		"for",
		"fxgroup",
		"GeometryShader",
		"groupshared",
		"half",
		"Hullshader",
		"if",
		"in",
		"inline",
		"inout",
		"InputPatch",
		"int",
		"interface",
		"line",
		"lineadj",
		"linear",
		"LineStream",
		"matrix",
		"min16float",
		"min10float",
		"min16int",
		"min12int",
		"min16uint",
		"namespace",
		"nointerpolation",
		"noperspective",
		"NULL",
		"out",
		"OutputPatch",
		"packoffset",
		"pass",
		"pixelfragment",
		"PixelShader",
		"point",
		"PointStream",
		"precise",
		"RasterizerState",
		"RenderTargetView",
		"return",
		"register",
		"row_major",
		"RWBuffer",
		"RWByteAddressBuffer",
		"RWStructuredBuffer",
		"RWTexture1D",
		"RWTexture1DArray",
		"RWTexture2D",
		"RWTexture2DArray",
		"RWTexture3D",
		"sample",
		"sampler",
		"SamplerState",
		"SamplerComparisonState",
		"shared",
		"snorm",
		"stateblock",
		"stateblock_state",
		"static",
		"string",
		"struct",
		"switch",
		"StructuredBuffer",
		"tbuffer",
		"technique",
		"technique10",
		"technique11",
		"texture",
		"Texture1D",
		"Texture1DArray",
		"Texture2D",
		"Texture2DArray",
		"Texture2DMS",
		"Texture2DMSArray",
		"Texture3D",
		"TextureCube",
		"TextureCubeArray",
		"true",
		"typedef",
		"triangle",
		"triangleadj",
		"TriangleStream",
		"uint",
		"uniform",
		"unorm",
		"unsigned",
		"vector",
		"vertexfragment",
		"VertexShader",
		"void",
		"volatile",
		"while",
		"float",
		"double",
		"int",
		"uint",
		"bool",
		"min10float",
		"min16float",
		"min12int",
		"min16float",
		"min16uint",
		"float1",
		"float2",
		"float3",
		"float4",
		"float1x1",
		"float1x2",
		"float1x3",
		"float1x4",
		"float2x1",
		"float2x2",
		"float2x3",
		"float2x4",
		"float3x1",
		"float3x2",
		"float3x3",
		"float3x4",
		"float4x1",
		"float4x2",
		"float4x3",
		"float4x4",
		"double1",
		"double2",
		"double3",
		"double4",
		"double1x1",
		"double1x2",
		"double1x3",
		"double1x4",
		"double2x1",
		"double2x2",
		"double2x3",
		"double2x4",
		"double3x1",
		"double3x2",
		"double3x3",
		"double3x4",
		"double4x1",
		"double4x2",
		"double4x3",
		"double4x4",
		"int1",
		"int2",
		"int3",
		"int4",
		"int1x1",
		"int1x2",
		"int1x3",
		"int1x4",
		"int2x1",
		"int2x2",
		"int2x3",
		"int2x4",
		"int3x1",
		"int3x2",
		"int3x3",
		"int3x4",
		"int4x1",
		"int4x2",
		"int4x3",
		"int4x4",
		"uint1",
		"uint2",
		"uint3",
		"uint4",
		"uint1x1",
		"uint1x2",
		"uint1x3",
		"uint1x4",
		"uint2x1",
		"uint2x2",
		"uint2x3",
		"uint2x4",
		"uint3x1",
		"uint3x2",
		"uint3x3",
		"uint3x4",
		"uint4x1",
		"uint4x2",
		"uint4x3",
		"uint4x4",
		"bool1",
		"bool2",
		"bool3",
		"bool4",
		"bool1x1",
		"bool1x2",
		"bool1x3",
		"bool1x4",
		"bool2x1",
		"bool2x2",
		"bool2x3",
		"bool2x4",
		"bool3x1",
		"bool3x2",
		"bool3x3",
		"bool3x4",
		"bool4x1",
		"bool4x2",
		"bool4x3",
		"bool4x4",
		"min10float1",
		"min10float2",
		"min10float3",
		"min10float4",
		"min10float1x1",
		"min10float1x2",
		"min10float1x3",
		"min10float1x4",
		"min10float2x1",
		"min10float2x2",
		"min10float2x3",
		"min10float2x4",
		"min10float3x1",
		"min10float3x2",
		"min10float3x3",
		"min10float3x4",
		"min10float4x1",
		"min10float4x2",
		"min10float4x3",
		"min10float4x4",
		"min16float1",
		"min16float2",
		"min16float3",
		"min16float4",
		"min16float1x1",
		"min16float1x2",
		"min16float1x3",
		"min16float1x4",
		"min16float2x1",
		"min16float2x2",
		"min16float2x3",
		"min16float2x4",
		"min16float3x1",
		"min16float3x2",
		"min16float3x3",
		"min16float3x4",
		"min16float4x1",
		"min16float4x2",
		"min16float4x3",
		"min16float4x4",
		"min12int1",
		"min12int2",
		"min12int3",
		"min12int4",
		"min12int1x1",
		"min12int1x2",
		"min12int1x3",
		"min12int1x4",
		"min12int2x1",
		"min12int2x2",
		"min12int2x3",
		"min12int2x4",
		"min12int3x1",
		"min12int3x2",
		"min12int3x3",
		"min12int3x4",
		"min12int4x1",
		"min12int4x2",
		"min12int4x3",
		"min12int4x4",
		"min16int1",
		"min16int2",
		"min16int3",
		"min16int4",
		"min16int1x1",
		"min16int1x2",
		"min16int1x3",
		"min16int1x4",
		"min16int2x1",
		"min16int2x2",
		"min16int2x3",
		"min16int2x4",
		"min16int3x1",
		"min16int3x2",
		"min16int3x3",
		"min16int3x4",
		"min16int4x1",
		"min16int4x2",
		"min16int4x3",
		"min16int4x4",
		"min16uint1",
		"min16uint2",
		"min16uint3",
		"min16uint4",
		"min16uint1x1",
		"min16uint1x2",
		"min16uint1x3",
		"min16uint1x4",
		"min16uint2x1",
		"min16uint2x2",
		"min16uint2x3",
		"min16uint2x4",
		"min16uint3x1",
		"min16uint3x2",
		"min16uint3x3",
		"min16uint3x4",
		"min16uint4x1",
		"min16uint4x2",
		"min16uint4x3",
		"min16uint4x4",
	};

	inline static int64_t
	_hlsl_buffer_position(HLSL& self)
	{
		return mn::stream_cursor_pos(self.out);
	}

	inline static bool
	_hlsl_code_generated_after(HLSL& self, int64_t pos)
	{
		return mn::stream_cursor_pos(self.out) > pos;
	}

	inline static void
	_hlsl_newline(HLSL& self)
	{
		mn::print_to(self.out, "\n");
		for (size_t i = 0; i < self.indent; ++i)
			mn::print_to(self.out, "\t");
	}

	inline static void
	_hlsl_enter_scope(HLSL& self, Scope* scope)
	{
		mn_assert(scope != nullptr);
		mn::buf_push(self.scope_stack, scope);
	}

	inline static void
	_hlsl_leave_scope(HLSL& self)
	{
		mn_assert(self.scope_stack.count > 1);
		mn::buf_pop(self.scope_stack);
	}

	inline static Scope*
	_hlsl_current_scope(HLSL& self)
	{
		return mn::buf_top(self.scope_stack);
	}

	inline static void
	_hlsl_loop_post_stmt_enter(HLSL& self, Stmt* post)
	{
		mn::buf_push(self.loop_post_stmt_stack, post);
	}

	inline static void
	_hlsl_loop_post_stmt_leave(HLSL& self)
	{
		mn::buf_pop(self.loop_post_stmt_stack);
	}

	inline static Stmt*
	_hlsl_current_loop_post_stmt(HLSL& self)
	{
		if (self.loop_post_stmt_stack.count > 0)
		{
			return mn::buf_top(self.loop_post_stmt_stack);
		}
		return nullptr;
	}

	inline static Symbol*
	_hlsl_find_symbol(HLSL& self, const char* name)
	{
		auto current_scope = _hlsl_current_scope(self);
		return scope_find(current_scope, name);
	}

	inline static const char*
	_hlsl_name(HLSL& self, const char* name)
	{
		if (auto it = mn::map_lookup(self.reserved_to_alternative, name))
		{
			if (it->value != nullptr)
				return it->value;

			mn::log_debug("reserved name collision: {}", name);
			auto str = mn::str_tmpf("RESERVED_{}", name);
			it->value = unit_intern(self.unit->parent_unit, str.ptr);
			return it->value;
		}

		return name;
	}

	inline static const char*
	_hlsl_symbol_name(HLSL& self, Symbol* sym, Decl* decl = nullptr);

	inline static const char*
	_hlsl_templated_type_name(HLSL& self, Type* type)
	{
		if (auto it = mn::map_lookup(self.template_mangled_names, type))
			return it->value;

		if (type->template_base_type)
		{
			auto res_str = mn::str_tmpf("{}", _hlsl_templated_type_name(self, type->template_base_type));
			if (type_is_templated(type) == false)
			{
				for (auto arg: type->template_base_args)
					res_str = mn::strf(res_str, "_{}", _hlsl_templated_type_name(self, arg));
			}
			auto res = _hlsl_name(self, res_str.ptr);
			mn::map_insert(self.template_mangled_names, type, res);
			return res;
		}

		auto res_str = mn::str_new();
		switch (type->kind)
		{
		case Type::KIND_VOID: res_str = mn::str_lit("void"); break;
		case Type::KIND_BOOL: res_str = mn::str_lit("bool"); break;
		case Type::KIND_INT: res_str = mn::str_lit("int"); break;
		case Type::KIND_UINT: res_str = mn::str_lit("uint"); break;
		case Type::KIND_FLOAT: res_str = mn::str_lit("float"); break;
		case Type::KIND_DOUBLE: res_str = mn::str_lit("double"); break;
		case Type::KIND_VEC:
			res_str = mn::strf(res_str, "{}{}", _hlsl_templated_type_name(self, type->vec.base), type->vec.width);
			break;
		case Type::KIND_MAT:
			res_str = mn::strf(res_str, "float{}x{}", _hlsl_templated_type_name(self, type->vec.base), type->mat.width);
			break;
		case Type::KIND_STRUCT:
			res_str = mn::str_lit(_hlsl_name(self, _hlsl_symbol_name(self, type->struct_type.symbol)));
			break;
		case Type::KIND_TEXTURE:
			if (type == type_texture1d)
			{
				res_str = mn::str_lit("Texture1D_float4");
			}
			else if (type == type_texture2d)
			{
				res_str = mn::str_lit("Texture2D_float4");
			}
			else if (type == type_texture3d)
			{
				res_str = mn::str_lit("Texture3D_float4");
			}
			else if (type == type_texture_cube)
			{
				res_str = mn::str_lit("TextureCube_float4");
			}
			else
			{
				mn_unreachable();
			}
			break;
		case Type::KIND_ARRAY:
			res_str = mn::strf(res_str, "array{}_{}", type->array.count, _hlsl_templated_type_name(self, type->array.base));
			break;
		case Type::KIND_ENUM:
			res_str = mn::str_lit(_hlsl_name(self, _hlsl_symbol_name(self, type->struct_type.symbol)));
			break;
		case Type::KIND_SAMPLER:
			res_str = mn::str_lit("sampler");
			break;
		case Type::KIND_TYPENAME:
			mn_unreachable_msg("codegen for typename types is not supported");
			break;
		default:
			mn_unreachable();
			break;
		}

		auto res = _hlsl_name(self, res_str.ptr);
		mn::map_insert(self.template_mangled_names, type, res);
		return res;
	}

	inline static const char*
	_hlsl_symbol_name(HLSL& self, Symbol* sym, Decl* decl)
	{
		bool use_raw_name = false;

		const char* res = sym->package_name;
		switch (sym->kind)
		{
		case Symbol::KIND_STRUCT_INSTANTIATION:
			res = _hlsl_templated_type_name(self, sym->type);
			break;
		case Symbol::KIND_FUNC_INSTANTIATION:
		{
			if (auto it = mn::map_lookup(self.template_mangled_names, (Type*)sym))
			{
				res = it->value;
				break;
			}
			// we only specialize if it's not a builtin function
			if (mn::map_lookup(sym->as_func_instantiation.decl->tags.table, KEYWORD_BUILTIN))
			{
				res = _hlsl_name(self, _hlsl_symbol_name(self, sym->as_func_instantiation.template_symbol));
			}
			else
			{
				auto str = mn::str_tmpf("{}", _hlsl_name(self, _hlsl_symbol_name(self, sym->as_func_instantiation.template_symbol)));
				for (auto t: sym->type->template_base_args)
					str = mn::strf(str, "_{}", _hlsl_templated_type_name(self, t));
				res = _hlsl_name(self, str.ptr);
			}
			mn::map_insert(self.template_mangled_names, (Type*)sym, res);
			break;
		}
		case Symbol::KIND_STRUCT:
			if (auto tag = mn::map_lookup(sym->struct_sym.decl->tags.table, KEYWORD_BUILTIN))
			{
				if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_HLSL))
				{
					if (arg->value.value->const_value.type == type_lit_string)
						res = arg->value.value->const_value.as_string;
					else
						res = sym->name;
				}
				else
				{
					res = sym->name;
				}
			}
			break;
		// in case the function is a builtin function we don't use it's package name
		case Symbol::KIND_FUNC:
			if (auto tag = mn::map_lookup(sym->func_sym.decl->tags.table, KEYWORD_BUILTIN))
			{
				if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_HLSL))
				{
					if (arg->value.value->const_value.type == type_lit_string)
						res = arg->value.value->const_value.as_string;
					else
						res = sym->name;
				}
				else
				{
					res = sym->name;
				}
			}
			break;
		case Symbol::KIND_FUNC_OVERLOAD_SET:
			if (decl)
			{
				if (auto tag = mn::map_lookup(decl->tags.table, KEYWORD_BUILTIN))
				{
					if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_HLSL))
					{
						if (arg->value.value->const_value.type == type_lit_string)
							res = arg->value.value->const_value.as_string;
						else
							res = sym->name;
					}
					else
					{
						res = sym->name;
					}
				}
			}
			else
			{
				// NOTE(Moustapha): this is very bad, we should know which decl we're using
				for (auto [decl, _]: sym->func_overload_set_sym.decls)
				{
					if (auto tag = mn::map_lookup(decl->tags.table, KEYWORD_BUILTIN))
					{
						if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_HLSL))
						{
							if (arg->value.value->const_value.type == type_lit_string)
								res = arg->value.value->const_value.as_string;
							else
								res = sym->name;
						}
						else
						{
							res = sym->name;
						}
						break;
					}
				}
			}
			break;
		default:
			break;
		}

		return res;
	}

	inline static mn::Str
	_hlsl_write_field(HLSL& self, mn::Str str, Type* type, const char* name)
	{
		bool can_write_name = false;
		switch (type->kind)
		{
		case Type::KIND_VOID:
			can_write_name = true;
			str = mn::strf(str, "void");
			break;
		case Type::KIND_BOOL:
			can_write_name = true;
			str = mn::strf(str, "bool");
			break;
		case Type::KIND_INT:
			can_write_name = true;
			str = mn::strf(str, "int");
			break;
		case Type::KIND_UINT:
			can_write_name = true;
			str = mn::strf(str, "uint");
			break;
		case Type::KIND_FLOAT:
			can_write_name = true;
			str = mn::strf(str, "float");
			break;
		case Type::KIND_DOUBLE:
			can_write_name = true;
			str = mn::strf(str, "double");
			break;
		case Type::KIND_VEC:
			can_write_name = true;
			if (type->vec.base == type_bool)
			{
				switch(type->vec.width)
				{
				case 2:
					str = mn::strf(str, "bool2");
					break;
				case 3:
					str = mn::strf(str, "bool3");
					break;
				case 4:
					str = mn::strf(str, "bool4");
					break;
				default:
					mn_unreachable();
					break;
				}
			}
			else if (type->vec.base == type_int)
			{
				switch(type->vec.width)
				{
				case 2:
					str = mn::strf(str, "int2");
					break;
				case 3:
					str = mn::strf(str, "int3");
					break;
				case 4:
					str = mn::strf(str, "int4");
					break;
				default:
					mn_unreachable();
					break;
				}
			}
			else if (type->vec.base == type_uint)
			{
				switch(type->vec.width)
				{
				case 2:
					str = mn::strf(str, "uint2");
					break;
				case 3:
					str = mn::strf(str, "uint3");
					break;
				case 4:
					str = mn::strf(str, "uint4");
					break;
				default:
					mn_unreachable();
					break;
				}
			}
			else if (type->vec.base == type_float)
			{
				switch(type->vec.width)
				{
				case 2:
					str = mn::strf(str, "float2");
					break;
				case 3:
					str = mn::strf(str, "float3");
					break;
				case 4:
					str = mn::strf(str, "float4");
					break;
				default:
					mn_unreachable();
					break;
				}
			}
			else if (type->vec.base == type_double)
			{
				switch(type->vec.width)
				{
				case 2:
					str = mn::strf(str, "double2");
					break;
				case 3:
					str = mn::strf(str, "double3");
					break;
				case 4:
					str = mn::strf(str, "double4");
					break;
				default:
					mn_unreachable();
					break;
				}
			}
			break;
		case Type::KIND_MAT:
			can_write_name = true;
			if (type == type_mat2)
			{
				str = mn::strf(str, "column_major float2x2");
			}
			else if (type == type_mat3)
			{
				str = mn::strf(str, "column_major float3x3");
			}
			else if (type == type_mat4)
			{
				str = mn::strf(str, "column_major float4x4");
			}
			else
			{
				mn_unreachable();
			}
			break;
		case Type::KIND_STRUCT:
		{
			can_write_name = true;
			const char* type_name = _hlsl_name(self, _hlsl_symbol_name(self, type->struct_type.symbol));
			if (type->template_base_type)
				type_name = _hlsl_templated_type_name(self, type);
			str = mn::strf(str, "{}", type_name);
			break;
		}
		case Type::KIND_TEXTURE:
			can_write_name = true;
			if (type->template_base_type)
			{
				if (type->template_base_type == type_texture1d)
				{
					str = mn::strf(str, "Texture1D");
				}
				else if (type->template_base_type == type_texture2d)
				{
					str = mn::strf(str, "Texture2D");
				}
				else if (type->template_base_type == type_texture3d)
				{
					str = mn::strf(str, "Texture3D");
				}
				else if (type->template_base_type == type_texture_cube)
				{
					str = mn::strf(str, "TextureCube");
				}
				else
				{
					mn_unreachable();
				}

				str = mn::strf(str, "<");
				for (size_t i = 0; i < type->template_base_args.count; ++i)
				{
					if (i > 0)
						str = mn::strf(", ");
					str = _hlsl_write_field(self, str, type->template_base_args[i], "");
				}
				str = mn::strf(str, ">");
			}
			else
			{
				if (type == type_texture1d)
				{
					str = mn::strf(str, "Texture1D<float4>");
				}
				else if (type == type_texture2d)
				{
					str = mn::strf(str, "Texture2D<float4>");
				}
				else if (type == type_texture3d)
				{
					str = mn::strf(str, "Texture3D<float4>");
				}
				else if (type == type_texture_cube)
				{
					str = mn::strf(str, "TextureCube<float4>");
				}
				else
				{
					mn_unreachable();
				}
			}
			break;
		case Type::KIND_ARRAY:
		{
			auto str_name = mn::str_lit(name);
			size_t count = 1;
			if (type_is_bounded_array(type))
				count = type->array.count;
			auto array_name = mn::str_tmpf("{}[{}]", str_name, count);
			str = _hlsl_write_field(self, str, type->array.base, array_name.ptr);
			break;
		}
		case Type::KIND_ENUM:
			can_write_name = true;
			str = mn::strf(str, "{}", _hlsl_name(self, _hlsl_symbol_name(self, type->enum_type.symbol)));
			break;
		case Type::KIND_SAMPLER:
			can_write_name = true;
			str = mn::strf(str, "SamplerState");
			break;
		case Type::KIND_TRIANGLE_STREAM:
			can_write_name = true;
			str = mn::strf(str, "TriangleStream<");
			for (size_t i = 0; i < type->template_base_args.count; ++i)
			{
				if (i > 0)
					str = mn::strf(", ");
				str = _hlsl_write_field(self, str, type->template_base_args[i], "");
			}
			str = mn::strf(str, ">");
			break;
		case Type::KIND_LINE_STREAM:
			can_write_name = true;
			str = mn::strf(str, "LineStream<");
			for (size_t i = 0; i < type->template_base_args.count; ++i)
			{
				if (i > 0)
					str = mn::strf(", ");
				str = _hlsl_write_field(self, str, type->template_base_args[i], "");
			}
			str = mn::strf(str, ">");
			break;
		case Type::KIND_POINT_STREAM:
			can_write_name = true;
			str = mn::strf(str, "PointStream<");
			for (size_t i = 0; i < type->template_base_args.count; ++i)
			{
				if (i > 0)
					str = mn::strf(", ");
				str = _hlsl_write_field(self, str, type->template_base_args[i], "");
			}
			str = mn::strf(str, ">");
			break;
		case Type::KIND_TYPENAME:
			mn_unreachable_msg("codegen for typename types is not supported");
			break;
		default:
			mn_unreachable();
			break;
		}

		auto str_name = mn::str_lit(name);
		if (can_write_name && str_name.count > 0)
		{
			if (str_name[0] == '[')
				str = mn::strf(str, "{}", str_name);
			else
				str = mn::strf(str, " {}", _hlsl_name(self, name));
		}
		return str;
	}

	inline static mn::Str
	_hlsl_write_field(HLSL& self, Type* type, const char* name)
	{
		return _hlsl_write_field(self, mn::str_tmp(), type, name);
	}

	inline static void
	_hlsl_zero_value(HLSL& self, Type* t)
	{
		switch (t->kind)
		{
		case Type::KIND_BOOL:
			mn::print_to(self.out, "false");
			break;
		case Type::KIND_INT:
		case Type::KIND_UINT:
			mn::print_to(self.out, "0");
			break;
		case Type::KIND_FLOAT:
		case Type::KIND_DOUBLE:
			mn::print_to(self.out, "0.0");
			break;
		case Type::KIND_VEC:
			mn::print_to(self.out, "{}(", _hlsl_write_field(self, t, nullptr));
			for (size_t i = 0; i < t->vec.width; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				_hlsl_zero_value(self, t->vec.base);
			}
			mn::print_to(self.out, ")");
			break;
		case Type::KIND_MAT:
			mn::print_to(self.out, "{}(", _hlsl_write_field(self, t, nullptr));
			for (size_t i = 0; i < t->mat.width * t->mat.width; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				_hlsl_zero_value(self, t->mat.base);
			}
			mn::print_to(self.out, ")");
			break;
		case Type::KIND_ARRAY:
			mn::print_to(self.out, "{{");
			for (size_t i = 0; i < t->array.count; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				_hlsl_zero_value(self, t->array.base);
			}
			mn::print_to(self.out, "}}");
			break;
		case Type::KIND_STRUCT:
			mn::print_to(self.out, "{{");
			for (size_t i = 0; i < t->struct_type.fields.count; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				if (t->struct_type.fields[i].default_value)
					hlsl_expr_gen(self, t->struct_type.fields[i].default_value);
				else
					_hlsl_zero_value(self, t->struct_type.fields[i].type);
			}
			mn::print_to(self.out, "}}");
			break;
		case Type::KIND_ENUM:
			mn::print_to(self.out, "{}(0)", _hlsl_write_field(self, t, nullptr));
			break;
		case Type::KIND_VOID:
		case Type::KIND_FUNC:
		case Type::KIND_TEXTURE:
		case Type::KIND_PACKAGE:
		case Type::KIND_FUNC_OVERLOAD_SET:
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_gen_atom_expr(HLSL& self, Expr* e)
	{
		if (e->atom.tkn.kind == Tkn::KIND_ID)
		{
			if (e->symbol)
			{
				auto package_name = mn::str_lit(_hlsl_symbol_name(self, e->symbol, e->atom.decl));
				if (package_name.count > 0)
				{
					mn::print_to(self.out, "{}", _hlsl_name(self, package_name.ptr));
					return;
				}
			}
		}

		if (e->atom.tkn.kind == Tkn::KIND_ID)
		{
			mn::print_to(self.out, "{}", _hlsl_name(self, e->atom.tkn.str));
		}
		else
		{
			mn::print_to(self.out, "{}", e->atom.tkn.str);
		}
	}

	inline static void
	_hlsl_gen_binary_expr(HLSL& self, Expr* e)
	{
		if (e->binary.op.kind == Tkn::KIND_STAR &&
			e->binary.left->type->kind == Type::KIND_MAT ||
			e->binary.right->type->kind == Type::KIND_MAT)
		{
			mn::print_to(self.out, "mul(");
			hlsl_expr_gen(self, e->binary.left);
			mn::print_to(self.out, ", ");
			hlsl_expr_gen(self, e->binary.right);
			mn::print_to(self.out, ")");
		}
		else
		{
			hlsl_expr_gen(self, e->binary.left);
			mn::print_to(self.out, " {} ", e->binary.op.str);
			hlsl_expr_gen(self, e->binary.right);
		}
	}

	inline static const char*
	_hlsl_tmp_name(HLSL& self)
	{
		auto scope = _hlsl_current_scope(self);
		auto res = mn::str_tmp();
		const char* interned_res = nullptr;
		while (true)
		{
			auto id = ++self.tmp_id;
			res = mn::strf(res, "_tmp_{}", id);
			interned_res = unit_intern(self.unit->parent_unit, res.ptr);
			if (scope_shallow_find(scope, interned_res) == nullptr)
				break;
			mn::str_clear(res);
		}
		return _hlsl_name(self, interned_res);
	}

	inline static const char*
	_hlsl_gen_compute_buffer_load(HLSL& self, Expr* e, const Buffer_Access_Info& info, Type* type)
	{
		mn_assert(info.size <= 16);

		auto name = _hlsl_tmp_name(self);

		mn::print_to(self.out, "{} = ", _hlsl_write_field(self, type, name));
		if (type_is_equal(type, type_float))
		{
			mn::print_to(self.out, "asfloat");
		}
		else if (type_is_equal(type, type_int))
		{
			mn::print_to(self.out, "int");
		}
		if (type_is_vec(type))
		{
			switch (type->vec.width)
			{
			case 2:
				if (type_is_equal(type, type_float))
				{
					mn::print_to(self.out, "asfloat");
				}
				else if (type_is_equal(type, type_int))
				{
					mn::print_to(self.out, "int2");
				}
				break;
			case 3:
				if (type_is_equal(type, type_float))
				{
					mn::print_to(self.out, "asfloat");
				}
				else if (type_is_equal(type, type_int))
				{
					mn::print_to(self.out, "int3");
				}
				break;
			case 4:
				if (type_is_equal(type, type_float))
				{
					mn::print_to(self.out, "asfloat");
				}
				else if (type_is_equal(type, type_int))
				{
					mn::print_to(self.out, "int4");
				}
				break;
			default:
				mn_unreachable();
				break;
			}
		}

		mn::print_to(self.out, "(");
		hlsl_expr_gen(self, info.buffer_name_expr);
		if (info.size == 4)
		{
			mn::print_to(self.out, ".Load({}", info.compile_time_offset);
			for (auto runtime_offset: info.runtime_offsets)
			{
				mn::print_to(self.out, " + ((");
				hlsl_expr_gen(self, runtime_offset.offset);
				mn::print_to(self.out, ") * {})", runtime_offset.type->unaligned_size);
			}
			mn::print_to(self.out, ")");
		}
		else if (info.size == 8)
		{
			mn::print_to(self.out, ".Load2({}", info.compile_time_offset);
			for (auto runtime_offset: info.runtime_offsets)
			{
				mn::print_to(self.out, " + ((");
				hlsl_expr_gen(self, runtime_offset.offset);
				mn::print_to(self.out, ") * {})", runtime_offset.type->unaligned_size);
			}
			mn::print_to(self.out, ")");
		}
		else if (info.size == 12)
		{
			mn::print_to(self.out, ".Load3({}", info.compile_time_offset);
			for (auto runtime_offset: info.runtime_offsets)
			{
				mn::print_to(self.out, " + ((");
				hlsl_expr_gen(self, runtime_offset.offset);
				mn::print_to(self.out, ") * {})", runtime_offset.type->unaligned_size);
			}
			mn::print_to(self.out, ")");
		}
		else if (info.size == 16)
		{
			mn::print_to(self.out, ".Load4({}", info.compile_time_offset);
			for (auto runtime_offset: info.runtime_offsets)
			{
				mn::print_to(self.out, " + ((");
				hlsl_expr_gen(self, runtime_offset.offset);
				mn::print_to(self.out, ") * {})", runtime_offset.type->unaligned_size);
			}
			mn::print_to(self.out, ")");
		}
		else
		{
			mn_unreachable();
		}
		mn::print_to(self.out, ")");
		mn::print_to(self.out, ";");
		_hlsl_newline(self);

		if (e != nullptr)
			mn::map_insert(self.symbol_to_names, (void*)e, name);
		return name;
	}

	inline static void
	_hlsl_gen_compute_buffer_store_operator(HLSL& self, Tkn::KIND op, const char* load_name, Expr* rhs)
	{
		switch (op)
		{
		// unary operations
		case Tkn::KIND_INC:
			mn::print_to(self.out, "{} + 1", load_name);
			break;
		case Tkn::KIND_DEC:
			mn::print_to(self.out, "{} - 1", load_name);
			break;
		// binary operations
		case Tkn::KIND_PLUS_EQUAL:
			mn::print_to(self.out, "{} + ", load_name);
			break;
		case Tkn::KIND_MINUS_EQUAL:
			mn::print_to(self.out, "{} - ", load_name);
			break;
		case Tkn::KIND_STAR_EQUAL:
			mn::print_to(self.out, "{} * ", load_name);
			break;
		case Tkn::KIND_DIVIDE_EQUAL:
			mn::print_to(self.out, "{} / ", load_name);
			break;
		case Tkn::KIND_MODULUS_EQUAL:
			mn::print_to(self.out, "{} % ", load_name);
			break;
		case Tkn::KIND_BIT_OR_EQUAL:
			mn::print_to(self.out, "{} | ", load_name);
			break;
		case Tkn::KIND_BIT_AND_EQUAL:
			mn::print_to(self.out, "{} & ", load_name);
			break;
		case Tkn::KIND_BIT_XOR_EQUAL:
			mn::print_to(self.out, "{} ^ ", load_name);
			break;
		case Tkn::KIND_BIT_SHIFT_LEFT_EQUAL:
			mn::print_to(self.out, "{} >> ", load_name);
			break;
		case Tkn::KIND_BIT_SHIFT_RIGHT_EQUAL:
			mn::print_to(self.out, "{} << ", load_name);
			break;
		case Tkn::KIND_BIT_NOT_EQUAL:
			mn::print_to(self.out, "{} ~ ", load_name);
			break;
		case Tkn::KIND_EQUAL:
			// equal is pure store with no loads
			break;
		default:
			mn_unreachable();
			break;
		}

		if (tkn_is_assign(op))
			hlsl_expr_gen(self, rhs);
	}

	inline static void
	_hlsl_gen_compute_buffer_store(HLSL& self, const Buffer_Access_Info& info, const char* load_name, Tkn::KIND op, Expr* rhs, Type* store_type)
	{
		mn_assert(info.size <= 16);

		hlsl_expr_gen(self, info.buffer_name_expr);
		if (info.size == 4)
		{
			mn::print_to(self.out, ".Store({}", info.compile_time_offset);
			for (auto runtime_offset: info.runtime_offsets)
			{
				mn::print_to(self.out, " + ((");
				hlsl_expr_gen(self, runtime_offset.offset);
				mn::print_to(self.out, ") * {})", runtime_offset.type->unaligned_size);
			}
			mn::print_to(self.out, ", ");

			if (store_type == type_float || store_type == type_lit_float)
			{
				mn::print_to(self.out, "asuint(");
			}
			else if (store_type == type_int || store_type == type_lit_int)
			{
				mn::print_to(self.out, "uint(");
			}
			_hlsl_gen_compute_buffer_store_operator(self, op, load_name, rhs);
			mn::print_to(self.out, ")");
		}
		else if (info.size == 8)
		{
			mn::print_to(self.out, ".Store2({}", info.compile_time_offset);
			for (auto runtime_offset: info.runtime_offsets)
			{
				mn::print_to(self.out, " + ((");
				hlsl_expr_gen(self, runtime_offset.offset);
				mn::print_to(self.out, ") * {})", runtime_offset.type->unaligned_size);
			}
			mn::print_to(self.out, ", ");

			if (store_type == type_float || store_type == type_lit_float)
			{
				mn::print_to(self.out, "asuint(");
			}
			else if (store_type == type_int || store_type == type_lit_int)
			{
				mn::print_to(self.out, "uint2(");
			}
			_hlsl_gen_compute_buffer_store_operator(self, op, load_name, rhs);
			mn::print_to(self.out, ")");
		}
		else if (info.size == 12)
		{
			mn::print_to(self.out, ".Store3({}", info.compile_time_offset);
			for (auto runtime_offset: info.runtime_offsets)
			{
				mn::print_to(self.out, " + ((");
				hlsl_expr_gen(self, runtime_offset.offset);
				mn::print_to(self.out, ") * {})", runtime_offset.type->unaligned_size);
			}
			mn::print_to(self.out, ", ");

			if (store_type == type_float || store_type == type_lit_float)
			{
				mn::print_to(self.out, "asuint(");
			}
			else if (store_type == type_int || store_type == type_lit_int)
			{
				mn::print_to(self.out, "uint3(");
			}
			_hlsl_gen_compute_buffer_store_operator(self, op, load_name, rhs);
			mn::print_to(self.out, ")");
		}
		else if (info.size == 16)
		{
			mn::print_to(self.out, ".Store4({}", info.compile_time_offset);
			for (auto runtime_offset: info.runtime_offsets)
			{
				mn::print_to(self.out, " + ((");
				hlsl_expr_gen(self, runtime_offset.offset);
				mn::print_to(self.out, ") * {})", runtime_offset.type->unaligned_size);
			}
			mn::print_to(self.out, ", ");

			if (store_type == type_float || store_type == type_lit_float)
			{
				mn::print_to(self.out, "asuint(");
			}
			else if (store_type == type_int || store_type == type_lit_int)
			{
				mn::print_to(self.out, "uint4(");
			}
			_hlsl_gen_compute_buffer_store_operator(self, op, load_name, rhs);
			mn::print_to(self.out, ")");
		}
		else
		{
			mn_unreachable();
		}
		mn::print_to(self.out, ")");
	}

	inline static void
	_hlsl_gen_unary_expr(HLSL& self, Expr* e)
	{
		if (e->unary.op.kind == Tkn::KIND_INC ||
			e->unary.op.kind == Tkn::KIND_DEC)
		{
			if (auto it = mn::map_lookup(self.buffer_access_info, e->unary.base); it && it->value.is_write)
			{
				auto load_name = mn::map_lookup(self.symbol_to_names, (void*)e->unary.base)->value;
				_hlsl_gen_compute_buffer_store(self, it->value, load_name, e->unary.op.kind, nullptr, e->type);
				return;
			}
		}

		mn::print_to(self.out, "{}", e->unary.op.str);
		hlsl_expr_gen(self, e->unary.base);
	}

	inline static void
	_hlsl_gen_dot_expr(HLSL& self, Expr* e)
	{
		auto lhs = e->dot.lhs;
		bool is_lhs_package = false;
		bool is_lhs_enum = type_is_enum(e->type);

		if (lhs && lhs->kind == Expr::KIND_ATOM)
			is_lhs_package = lhs->symbol->kind == Symbol::KIND_PACKAGE;

		if (is_lhs_package)
		{
			hlsl_expr_gen(self, e->dot.rhs);
		}
		else if (is_lhs_enum)
		{
			// glslang hlsl implementation doesn't work with enums so for now we generate them as macros an integers
			auto enum_name = _hlsl_symbol_name(self, e->type->enum_type.symbol);
			mn::print_to(self.out, "{}_", enum_name);
			hlsl_expr_gen(self, e->dot.rhs);
		}
		else if (auto it = mn::map_lookup(self.symbol_to_names, (void*)e))
		{
			mn::print_to(self.out, "{}", it->value);
		}
		else
		{
			hlsl_expr_gen(self, e->dot.lhs);
			const char* dot = ".";
			if (e->dot.lhs->symbol)
				if (auto decl = symbol_decl(e->dot.lhs->symbol))
					if (auto it = mn::map_lookup(decl->tags.table, KEYWORD_UNIFORM))
						dot = "_";
			mn::print_to(self.out, "{}", dot);
			hlsl_expr_gen(self, e->dot.rhs);
		}
	}

	inline static void
	_hlsl_gen_indexed_expr(HLSL& self, Expr* e)
	{
		if (auto it = mn::map_lookup(self.symbol_to_names, (void*)e))
		{
			mn::print_to(self.out, "{}", it->value);
		}
		else
		{
			hlsl_expr_gen(self, e->indexed.base);
			mn::print_to(self.out, "[");
			hlsl_expr_gen(self, e->indexed.index);
			mn::print_to(self.out, "]");
		}
	}

	inline static void
	_hlsl_gen_call_expr(HLSL& self, Expr* e)
	{
		bool is_method = false;
		if (auto d = e->call.func)
		{
			is_method = mn::map_lookup(d->tags.table, KEYWORD_HLSL_METHOD) != nullptr;
		}

		if (is_method && e->call.args.count > 0)
		{
			hlsl_expr_gen(self, e->call.args[0]);
			mn::print_to(self.out, ".");
			hlsl_expr_gen(self, e->call.base);
			mn::print_to(self.out, "(");
			for (size_t i = 1; i < e->call.args.count; ++i)
			{
				if (i > 1)
					mn::print_to(self.out, ", ");
				hlsl_expr_gen(self, e->call.args[i]);
			}
			mn::print_to(self.out, ")");
		}
		else
		{
			hlsl_expr_gen(self, e->call.base);
			mn::print_to(self.out, "(");
			for (size_t i = 0; i < e->call.args.count; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				hlsl_expr_gen(self, e->call.args[i]);
			}
			mn::print_to(self.out, ")");
		}
	}

	inline static void
	_hlsl_gen_cast_expr(HLSL& self, Expr* e)
	{
		mn::print_to(self.out, "{}(", _hlsl_write_field(self, e->type, ""));
		hlsl_expr_gen(self, e->cast.base);
		mn::print_to(self.out, ")");
	}

	inline static void
	_hlsl_gen_complit_expr(HLSL& self, Expr* e)
	{
		if (auto it = mn::map_lookup(self.symbol_to_names, (void*)e))
		{
			mn::print_to(self.out, it->value);
		}
		else
		{
			mn_unreachable();
		}
	}

	inline static void
	_hlsl_rewrite_complits_in_expr(HLSL& self, Expr* e, bool is_const);

	inline static void
	_hlsl_rewrite_complits_in_binary_expr(HLSL& self, Expr* e, bool is_const)
	{
		_hlsl_rewrite_complits_in_expr(self, e->binary.left, is_const);
		_hlsl_rewrite_complits_in_expr(self, e->binary.right, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_unary_expr(HLSL& self, Expr* e, bool is_const)
	{
		_hlsl_rewrite_complits_in_expr(self, e->unary.base, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_dot_expr(HLSL& self, Expr* e, bool is_const)
	{
		if (e->dot.lhs)
			_hlsl_rewrite_complits_in_expr(self, e->dot.lhs, is_const);
		_hlsl_rewrite_complits_in_expr(self, e->dot.rhs, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_indexed_expr(HLSL& self, Expr* e, bool is_const)
	{
		_hlsl_rewrite_complits_in_expr(self, e->indexed.base, is_const);
		_hlsl_rewrite_complits_in_expr(self, e->indexed.index, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_call_expr(HLSL& self, Expr* e, bool is_const)
	{
		_hlsl_rewrite_complits_in_expr(self, e->call.base, is_const);
		for (auto arg: e->call.args)
			_hlsl_rewrite_complits_in_expr(self, arg, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_cast_expr(HLSL& self, Expr* e, bool is_const)
	{
		_hlsl_rewrite_complits_in_expr(self, e->cast.base, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_complit_expr(HLSL& self, Expr* e, bool is_const)
	{
		for (size_t i = 0; i < e->complit.fields.count; ++i)
		{
			if (e->complit.fields[i].value)
				_hlsl_rewrite_complits_in_expr(self, e->complit.fields[i].value, is_const);
		}

		if (is_const)
			mn::print_to(self.out, "static const ");

		// handle arrays differently
		if (type_is_array(e->type))
		{
			auto tmp_name = _hlsl_tmp_name(self);
			mn::map_insert(self.symbol_to_names, (void*)e, tmp_name);
			mn::print_to(self.out, "{} = {{", _hlsl_write_field(self, e->type, tmp_name));
			for (size_t i = 0; i < e->type->array.count; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				if (i < e->complit.fields.count)
					hlsl_expr_gen(self, e->complit.fields[i].value);
				else
					_hlsl_zero_value(self, e->type->array.base);
			}
			mn::print_to(self.out, "}};");
			_hlsl_newline(self);
		}
		else if (type_is_matrix(e->type))
		{
			auto tmp_name = _hlsl_tmp_name(self);
			mn::map_insert(self.symbol_to_names, (void*)e, tmp_name);
			mn::print_to(self.out, "{} = {{", _hlsl_write_field(self, e->type, tmp_name));
			for (size_t i = 0; i < e->type->mat.width; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				if (i < e->complit.fields.count)
					hlsl_expr_gen(self, e->complit.fields[i].value);
				else
					_hlsl_zero_value(self, e->type->mat.base);
			}
			mn::print_to(self.out, "}};");
			_hlsl_newline(self);
		}
		else if (type_is_vec(e->type))
		{
			auto tmp_name = _hlsl_tmp_name(self);
			mn::map_insert(self.symbol_to_names, (void*)e, tmp_name);
			mn::print_to(self.out, "{} = {}(", _hlsl_write_field(self, e->type, tmp_name), _hlsl_write_field(self, e->type, nullptr));
			for (size_t i = 0; i < e->type->vec.width; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");

				if (auto field_it = mn::map_lookup(e->complit.referenced_fields, i))
				{
					auto field = e->complit.fields[field_it->value];
					hlsl_expr_gen(self, field.value);
					// advance the field index by sub vector size to handle vector upcast cases
					if (type_is_vec(field.value->type))
						i += field.value->type->vec.width - 1;
				}
				else
				{
					_hlsl_zero_value(self, e->type->vec.base);
				}
			}
			mn::print_to(self.out, ");");
			_hlsl_newline(self);
		}
		else if (type_is_struct(e->type))
		{
			auto tmp_name = _hlsl_tmp_name(self);
			mn::map_insert(self.symbol_to_names, (void*)e, tmp_name);
			mn::print_to(self.out, "{} = {{", _hlsl_write_field(self, e->type, tmp_name));
			for (size_t i = 0; i < e->type->struct_type.fields.count; ++i)
			{
				const auto& struct_field = e->type->struct_type.fields[i];
				if (i > 0)
					mn::print_to(self.out, ", ");

				if (auto field_it = mn::map_lookup(e->complit.referenced_fields, i))
				{
					auto field = e->complit.fields[field_it->value];
					hlsl_expr_gen(self, field.value);
				}
				else
				{
					if (struct_field.default_value)
						hlsl_expr_gen(self, struct_field.default_value);
					else
						_hlsl_zero_value(self, struct_field.type);
				}
			}
			mn::print_to(self.out, "}};");
			_hlsl_newline(self);
		}
		else
		{
			mn_unreachable();
		}
	}

	inline static void
	_hlsl_rewrite_complits_in_expr(HLSL& self, Expr* e, bool is_const)
	{
		switch (e->kind)
		{
		case Expr::KIND_ATOM:
			// do nothing, no complit here
			break;
		case Expr::KIND_BINARY:
			_hlsl_rewrite_complits_in_binary_expr(self, e, is_const);
			break;
		case Expr::KIND_UNARY:
			_hlsl_rewrite_complits_in_unary_expr(self, e, is_const);
			break;
		case Expr::KIND_DOT:
			_hlsl_rewrite_complits_in_dot_expr(self, e, is_const);
			break;
		case Expr::KIND_INDEXED:
			_hlsl_rewrite_complits_in_indexed_expr(self, e, is_const);
			break;
		case Expr::KIND_CALL:
			_hlsl_rewrite_complits_in_call_expr(self, e, is_const);
			break;
		case Expr::KIND_CAST:
			_hlsl_rewrite_complits_in_cast_expr(self, e, is_const);
			break;
		case Expr::KIND_COMPLIT:
			_hlsl_rewrite_complits_in_complit_expr(self, e, is_const);
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_rewrite_complits_in_return_stmt(HLSL& self, Stmt* s, bool is_const)
	{
		_hlsl_rewrite_complits_in_expr(self, s->return_stmt, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_if_stmt(HLSL& self, Stmt* s, bool is_const)
	{
		for (auto cond: s->if_stmt.cond)
			_hlsl_rewrite_complits_in_expr(self, cond, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_stmt(HLSL& self, Stmt* s, bool is_const);

	inline static void
	_hlsl_rewrite_complits_in_for_stmt(HLSL& self, Stmt* s, bool is_const)
	{
		if (s->for_stmt.init)
			_hlsl_rewrite_complits_in_stmt(self, s->for_stmt.init, is_const);
		if (s->for_stmt.cond)
			_hlsl_rewrite_complits_in_expr(self, s->for_stmt.cond, is_const);
		if (s->for_stmt.post)
			_hlsl_rewrite_complits_in_stmt(self, s->for_stmt.post, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_assign_stmt(HLSL& self, Stmt* s, bool is_const)
	{
		for (auto e: s->assign_stmt.lhs)
			_hlsl_rewrite_complits_in_expr(self, e, is_const);

		for (auto e: s->assign_stmt.rhs)
			_hlsl_rewrite_complits_in_expr(self, e, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_block_stmt(HLSL& self, Stmt* s, bool is_const)
	{
		for (auto stmt: s->block_stmt)
			_hlsl_rewrite_complits_in_stmt(self, stmt, is_const);
	}

	inline static void
	_hlsl_rewrite_complits_in_decl_stmt(HLSL& self, Stmt* s, bool is_const)
	{
		auto scope = _hlsl_current_scope(self);
		auto d = s->decl_stmt;
		switch (d->kind)
		{
		case Decl::KIND_VAR:
		{
			for (size_t i = 0; i < d->var_decl.names.count; ++i)
			{
				auto name = d->var_decl.names[i];
				if (auto sym = scope_find(scope, name.str))
				{
					if (sym->var_sym.value)
						_hlsl_rewrite_complits_in_expr(self, sym->var_sym.value, is_const);
				}
			}
			break;
		}
		case Decl::KIND_CONST:
		{
			for (size_t i = 0; i < d->const_decl.names.count; ++i)
			{
				auto name = d->const_decl.names[i];
				if (auto sym = scope_find(scope, name.str))
				{
					if (sym->const_sym.value)
						_hlsl_rewrite_complits_in_expr(self, sym->const_sym.value, is_const);
				}
			}
			break;
		}
		case Decl::KIND_FUNC:
			// ignore it, we'll rewrite complits when we generate the function itself
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_rewrite_complits_in_stmt(HLSL& self, Stmt* s, bool is_const)
	{
		switch (s->kind)
		{
		case Stmt::KIND_BREAK:
		case Stmt::KIND_CONTINUE:
		case Stmt::KIND_DISCARD:
			// do nothing, no complits here
			break;
		case Stmt::KIND_RETURN:
			_hlsl_rewrite_complits_in_return_stmt(self, s, is_const);
			break;
		case Stmt::KIND_IF:
			_hlsl_rewrite_complits_in_if_stmt(self, s, is_const);
			break;
		case Stmt::KIND_FOR:
			_hlsl_rewrite_complits_in_for_stmt(self, s, is_const);
			break;
		case Stmt::KIND_ASSIGN:
			_hlsl_rewrite_complits_in_assign_stmt(self, s, is_const);
			break;
		case Stmt::KIND_EXPR:
			_hlsl_rewrite_complits_in_expr(self, s->expr_stmt, is_const);
			break;
		case Stmt::KIND_BLOCK:
			_hlsl_rewrite_complits_in_block_stmt(self, s, is_const);
			break;
		case Stmt::KIND_DECL:
			_hlsl_rewrite_complits_in_decl_stmt(self, s, is_const);
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_stmt(HLSL& self, Stmt* s);

	inline static void
	_hlsl_rewrite_buffer_access_in_expr(HLSL& self, Expr* e, bool is_write);

	inline static void
	_hlsl_rewrite_buffer_access_in_binary_expr(HLSL& self, Expr* e, bool is_write)
	{
		_hlsl_rewrite_buffer_access_in_expr(self, e->binary.left, is_write);
		_hlsl_rewrite_buffer_access_in_expr(self, e->binary.right, is_write);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_unary_expr(HLSL& self, Expr* e, bool is_write)
	{
		if (e->unary.op.kind == Tkn::KIND_INC ||
			e->unary.op.kind == Tkn::KIND_DEC)
		{
			_hlsl_rewrite_buffer_access_in_expr(self, e->unary.base, true);
		}
		else
		{
			_hlsl_rewrite_buffer_access_in_expr(self, e->unary.base, is_write);
		}
	}

	inline static bool
	_hlsl_expr_is_buffer(Expr* e)
	{
		return (
			e->symbol &&
			e->symbol->kind == Symbol::KIND_VAR &&
			e->symbol->var_sym.is_buffer
		);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_dot_expr(HLSL& self, Expr* e, bool is_write)
	{
		if (e->dot.lhs == nullptr)
			return;

		_hlsl_rewrite_buffer_access_in_expr(self, e->dot.lhs, is_write);

		if (auto it = mn::map_lookup(self.buffer_access_info, e->dot.lhs))
		{
			mn_assert(e->dot.has_offset);

			if (type_is_vec(e->type))
			{
				// do nothing
			}
			else
			{
				Buffer_Access_Info info{};
				info.buffer_name_expr = it->value.buffer_name_expr;
				info.compile_time_offset = it->value.compile_time_offset + e->dot.unaligned_offset;
				info.runtime_offsets = mn::buf_memcpy_clone(it->value.runtime_offsets, self.arena);
				info.size = e->type->unaligned_size;
				info.is_write = it->value.is_write;
				mn::map_insert(self.buffer_access_info, e, info);
			}
		}
		else if (_hlsl_expr_is_buffer(e->dot.lhs))
		{
			mn_assert(e->dot.has_offset);

			Buffer_Access_Info info{};
			info.buffer_name_expr = e->dot.lhs;
			info.compile_time_offset = e->dot.unaligned_offset;
			info.runtime_offsets = mn::buf_with_allocator<Buffer_Access_Runtime_Offset>(self.arena);
			info.size = e->type->unaligned_size;
			info.is_write = is_write;
			mn::map_insert(self.buffer_access_info, e, info);
		}
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_indexed_expr(HLSL& self, Expr* e, bool is_write)
	{
		_hlsl_rewrite_buffer_access_in_expr(self, e->indexed.base, is_write);
		_hlsl_rewrite_buffer_access_in_expr(self, e->indexed.index, is_write);

		if (auto it = mn::map_lookup(self.buffer_access_info, e->indexed.base))
		{
			Buffer_Access_Info info{};
			info.buffer_name_expr = it->value.buffer_name_expr;
			info.compile_time_offset = it->value.compile_time_offset;

			Buffer_Access_Runtime_Offset runtime_offset{};
			runtime_offset.offset = e->indexed.index;
			runtime_offset.type = e->type;
			info.runtime_offsets = mn::buf_with_allocator<Buffer_Access_Runtime_Offset>(self.arena);
			mn::buf_push(info.runtime_offsets, runtime_offset);

			info.size = e->type->unaligned_size;
			info.is_write = it->value.is_write;
			mn::map_insert(self.buffer_access_info, e, info);
		}
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_call_expr(HLSL& self, Expr* e, bool is_write)
	{
		_hlsl_rewrite_buffer_access_in_expr(self, e->call.base, is_write);
		for (auto arg: e->call.args)
			_hlsl_rewrite_buffer_access_in_expr(self, arg, is_write);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_cast_expr(HLSL& self, Expr* e, bool is_write)
	{
		_hlsl_rewrite_buffer_access_in_expr(self, e->cast.base, is_write);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_complit_expr(HLSL& self, Expr* e, bool is_write)
	{
		for (auto field: e->complit.fields)
			_hlsl_rewrite_buffer_access_in_expr(self, field.value, is_write);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_expr(HLSL& self, Expr* e, bool is_write)
	{
		switch (e->kind)
		{
		case Expr::KIND_ATOM:
			// do nothing, no buffer access here
			break;
		case Expr::KIND_BINARY:
			_hlsl_rewrite_buffer_access_in_binary_expr(self, e, is_write);
			break;
		case Expr::KIND_UNARY:
			_hlsl_rewrite_buffer_access_in_unary_expr(self, e, is_write);
			break;
		case Expr::KIND_DOT:
			_hlsl_rewrite_buffer_access_in_dot_expr(self, e, is_write);
			break;
		case Expr::KIND_INDEXED:
			_hlsl_rewrite_buffer_access_in_indexed_expr(self, e, is_write);
			break;
		case Expr::KIND_CALL:
			_hlsl_rewrite_buffer_access_in_call_expr(self, e, is_write);
			break;
		case Expr::KIND_CAST:
			_hlsl_rewrite_buffer_access_in_cast_expr(self, e, is_write);
			break;
		case Expr::KIND_COMPLIT:
			_hlsl_rewrite_buffer_access_in_complit_expr(self, e, is_write);
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_return_stmt(HLSL& self, Stmt* s)
	{
		if (s->return_stmt)
			_hlsl_rewrite_buffer_access_in_expr(self, s->return_stmt, false);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_if_stmt(HLSL& self, Stmt* s)
	{
		for (auto cond: s->if_stmt.cond)
			_hlsl_rewrite_buffer_access_in_expr(self, cond, false);
		for (auto body: s->if_stmt.body)
			_hlsl_rewrite_buffer_access_in_stmt(self, body);
		if (s->if_stmt.else_body)
			_hlsl_rewrite_buffer_access_in_stmt(self, s->if_stmt.else_body);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_for_stmt(HLSL& self, Stmt* s)
	{
		if (s->for_stmt.init)
			_hlsl_rewrite_buffer_access_in_stmt(self, s->for_stmt.init);
		if (s->for_stmt.cond)
			_hlsl_rewrite_buffer_access_in_expr(self, s->for_stmt.cond, false);
		if (s->for_stmt.post)
			_hlsl_rewrite_buffer_access_in_stmt(self, s->for_stmt.post);
		if (s->for_stmt.body)
			_hlsl_rewrite_buffer_access_in_stmt(self, s->for_stmt.body);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_assign_stmt(HLSL& self, Stmt* s)
	{
		for (auto e: s->assign_stmt.lhs)
			_hlsl_rewrite_buffer_access_in_expr(self, e, true);

		for (auto e: s->assign_stmt.rhs)
			_hlsl_rewrite_buffer_access_in_expr(self, e, false);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_block_stmt(HLSL& self, Stmt* s)
	{
		for (auto stmt: s->block_stmt)
			_hlsl_rewrite_buffer_access_in_stmt(self, stmt);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_decl_stmt(HLSL& self, Stmt* s)
	{
		auto d = s->decl_stmt;
		switch (d->kind)
		{
		case Decl::KIND_VAR:
		{
			for (auto e: d->var_decl.values)
				_hlsl_rewrite_buffer_access_in_expr(self, e, false);
			break;
		}
		case Decl::KIND_CONST:
			// ignore it, constants can't be assigned a buffer value
			break;
		case Decl::KIND_FUNC:
			// ignore it, we'll rewrite buffer access when we generate the function itself
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_stmt(HLSL& self, Stmt* s)
	{
		switch (s->kind)
		{
		case Stmt::KIND_BREAK:
		case Stmt::KIND_CONTINUE:
		case Stmt::KIND_DISCARD:
			// do nothing, no buffer access here
			break;
		case Stmt::KIND_RETURN:
			_hlsl_rewrite_buffer_access_in_return_stmt(self, s);
			break;
		case Stmt::KIND_IF:
			_hlsl_rewrite_buffer_access_in_if_stmt(self, s);
			break;
		case Stmt::KIND_FOR:
			_hlsl_rewrite_buffer_access_in_for_stmt(self, s);
			break;
		case Stmt::KIND_ASSIGN:
			_hlsl_rewrite_buffer_access_in_assign_stmt(self, s);
			break;
		case Stmt::KIND_EXPR:
			_hlsl_rewrite_buffer_access_in_expr(self, s->expr_stmt, false);
			break;
		case Stmt::KIND_BLOCK:
			_hlsl_rewrite_buffer_access_in_block_stmt(self, s);
			break;
		case Stmt::KIND_DECL:
			_hlsl_rewrite_buffer_access_in_decl_stmt(self, s);
			break;
		default:
			mn_unreachable();
			break;
		}
	}


	inline static void
	_hlsl_rewrite_buffer_access_in_expr_gen(HLSL& self, Expr* e, bool write);

	inline static void
	_hlsl_rewrite_buffer_access_in_binary_expr_gen(HLSL& self, Expr* e, bool write)
	{
		_hlsl_rewrite_buffer_access_in_expr_gen(self, e->binary.left, write);
		_hlsl_rewrite_buffer_access_in_expr_gen(self, e->binary.right, write);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_unary_expr_gen(HLSL& self, Expr* e, bool write)
	{
		_hlsl_rewrite_buffer_access_in_expr_gen(self, e->unary.base, write);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_dot_expr_gen(HLSL& self, Expr* e, bool write)
	{
		if (e->dot.lhs == nullptr)
			return;

		if (auto it = mn::map_lookup(self.buffer_access_info, e))
		{
			if (write == false)
				_hlsl_gen_compute_buffer_load(self, e, it->value, e->type);
		}
		else
		{
			_hlsl_rewrite_buffer_access_in_expr_gen(self, e->dot.lhs, write);
		}
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_indexed_expr_gen(HLSL& self, Expr* e, bool write)
	{
		_hlsl_rewrite_buffer_access_in_expr_gen(self, e->indexed.index, false);

		if (auto it = mn::map_lookup(self.buffer_access_info, e))
		{
			if (write == false)
				_hlsl_gen_compute_buffer_load(self, e, it->value, e->type);
		}
		else
		{
			_hlsl_rewrite_buffer_access_in_expr_gen(self, e->indexed.base, false);
		}
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_call_expr_gen(HLSL& self, Expr* e, bool write)
	{
		_hlsl_rewrite_buffer_access_in_expr_gen(self, e->call.base, write);
		for (auto arg: e->call.args)
			_hlsl_rewrite_buffer_access_in_expr_gen(self, arg, write);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_cast_expr_gen(HLSL& self, Expr* e, bool write)
	{
		_hlsl_rewrite_buffer_access_in_expr_gen(self, e->cast.base, write);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_complit_expr_gen(HLSL& self, Expr* e, bool write)
	{
		for (auto field: e->complit.fields)
			_hlsl_rewrite_buffer_access_in_expr_gen(self, field.value, write);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_expr_gen(HLSL& self, Expr* e, bool write)
	{
		switch (e->kind)
		{
		case Expr::KIND_ATOM:
			// do nothing, no buffer access here
			break;
		case Expr::KIND_BINARY:
			_hlsl_rewrite_buffer_access_in_binary_expr_gen(self, e, write);
			break;
		case Expr::KIND_UNARY:
			_hlsl_rewrite_buffer_access_in_unary_expr_gen(self, e, write);
			break;
		case Expr::KIND_DOT:
			_hlsl_rewrite_buffer_access_in_dot_expr_gen(self, e, write);
			break;
		case Expr::KIND_INDEXED:
			_hlsl_rewrite_buffer_access_in_indexed_expr_gen(self, e, write);
			break;
		case Expr::KIND_CALL:
			_hlsl_rewrite_buffer_access_in_call_expr_gen(self, e, write);
			break;
		case Expr::KIND_CAST:
			_hlsl_rewrite_buffer_access_in_cast_expr_gen(self, e, write);
			break;
		case Expr::KIND_COMPLIT:
			_hlsl_rewrite_buffer_access_in_complit_expr_gen(self, e, write);
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_stmt_gen(HLSL& self, Stmt* s);

	inline static void
	_hlsl_rewrite_buffer_access_in_return_stmt_gen(HLSL& self, Stmt* s)
	{
		if (s->return_stmt)
			_hlsl_rewrite_buffer_access_in_expr_gen(self, s->return_stmt, false);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_if_stmt_gen(HLSL& self, Stmt* s)
	{
		for (auto cond: s->if_stmt.cond)
			_hlsl_rewrite_buffer_access_in_expr_gen(self, cond, false);
		for (auto body: s->if_stmt.body)
			_hlsl_rewrite_buffer_access_in_stmt_gen(self, body);
		if (s->if_stmt.else_body)
			_hlsl_rewrite_buffer_access_in_stmt_gen(self, s->if_stmt.else_body);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_for_stmt_gen(HLSL& self, Stmt* s)
	{
		if (s->for_stmt.init)
			_hlsl_rewrite_buffer_access_in_stmt_gen(self, s->for_stmt.init);
		if (s->for_stmt.cond)
			_hlsl_rewrite_buffer_access_in_expr_gen(self, s->for_stmt.cond, false);
		if (s->for_stmt.post)
			_hlsl_rewrite_buffer_access_in_stmt_gen(self, s->for_stmt.post);
		if (s->for_stmt.body)
			_hlsl_rewrite_buffer_access_in_stmt_gen(self, s->for_stmt.body);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_assign_stmt_gen(HLSL& self, Stmt* s)
	{
		for (auto e: s->assign_stmt.lhs)
			_hlsl_rewrite_buffer_access_in_expr_gen(self, e, true);

		for (auto e: s->assign_stmt.rhs)
			_hlsl_rewrite_buffer_access_in_expr_gen(self, e, false);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_block_stmt_gen(HLSL& self, Stmt* s)
	{
		for (auto stmt: s->block_stmt)
			_hlsl_rewrite_buffer_access_in_stmt_gen(self, stmt);
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_decl_stmt_gen(HLSL& self, Stmt* s)
	{
		auto d = s->decl_stmt;
		switch (d->kind)
		{
		case Decl::KIND_VAR:
		{
			for (auto e: d->var_decl.values)
				_hlsl_rewrite_buffer_access_in_expr_gen(self, e, false);
			break;
		}
		case Decl::KIND_CONST:
			// ignore it, constants can't be assigned a buffer value
			break;
		case Decl::KIND_FUNC:
			// ignore it, we'll rewrite buffer access when we generate the function itself
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_rewrite_buffer_access_in_stmt_gen(HLSL& self, Stmt* s)
	{
		switch (s->kind)
		{
		case Stmt::KIND_BREAK:
		case Stmt::KIND_CONTINUE:
		case Stmt::KIND_DISCARD:
			// do nothing, no buffer access here
			break;
		case Stmt::KIND_RETURN:
			_hlsl_rewrite_buffer_access_in_return_stmt_gen(self, s);
			break;
		case Stmt::KIND_IF:
			_hlsl_rewrite_buffer_access_in_if_stmt_gen(self, s);
			break;
		case Stmt::KIND_FOR:
			_hlsl_rewrite_buffer_access_in_for_stmt_gen(self, s);
			break;
		case Stmt::KIND_ASSIGN:
			_hlsl_rewrite_buffer_access_in_assign_stmt_gen(self, s);
			break;
		case Stmt::KIND_EXPR:
			_hlsl_rewrite_buffer_access_in_expr_gen(self, s->expr_stmt, false);
			break;
		case Stmt::KIND_BLOCK:
			_hlsl_rewrite_buffer_access_in_block_stmt_gen(self, s);
			break;
		case Stmt::KIND_DECL:
			_hlsl_rewrite_buffer_access_in_decl_stmt_gen(self, s);
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static bool
	_hlsl_add_semicolon_after(HLSL& self, Stmt* s)
	{
		if (s->kind == Stmt::KIND_BREAK ||
			s->kind == Stmt::KIND_CONTINUE ||
			s->kind == Stmt::KIND_DISCARD ||
			s->kind == Stmt::KIND_RETURN ||
			s->kind == Stmt::KIND_ASSIGN ||
			s->kind == Stmt::KIND_EXPR)
		{
			return true;
		}
		return false;
	}

	inline static void
	_hlsl_gen_break_stmt(HLSL& self, Stmt* s)
	{
		mn::print_to(self.out, "break");
	}

	inline static void
	_hlsl_gen_continue_stmt(HLSL& self, Stmt* s)
	{
		if (auto post = _hlsl_current_loop_post_stmt(self))
		{
			hlsl_stmt_gen(self, post);
			if (_hlsl_add_semicolon_after(self, post))
			{
				mn::print_to(self.out, ";");
			}
			_hlsl_newline(self);
		}
		mn::print_to(self.out, "continue");
	}

	inline static void
	_hlsl_gen_discard_stmt(HLSL& self, Stmt* s)
	{
		mn::print_to(self.out, "discard");
	}

	inline static void
	_hlsl_gen_return_stmt(HLSL& self, Stmt* s)
	{
		if (s->return_stmt)
		{
			mn::print_to(self.out, "return ");
			hlsl_expr_gen(self, s->return_stmt);
		}
		else
		{
			mn::print_to(self.out, "return");
		}
	}

	inline static void
	_hlsl_gen_block_stmt(HLSL& self, Stmt* s)
	{
		auto scope = unit_scope_find(self.unit->parent_unit, s);
		if (scope)
			_hlsl_enter_scope(self, scope);

		mn::print_to(self.out, "{{");
		++self.indent;

		for (auto stmt: s->block_stmt)
		{
			_hlsl_newline(self);
			hlsl_stmt_gen(self, stmt);
			if (_hlsl_add_semicolon_after(self, stmt))
			{
				mn::print_to(self.out, ";");
			}
		}

		--self.indent;
		_hlsl_newline(self);
		mn::print_to(self.out, "}}");

		if (scope)
			_hlsl_leave_scope(self);
	}

	inline static void
	_hlsl_gen_if_stmt(HLSL& self, Stmt* s)
	{
		for (size_t i = 0; i < s->if_stmt.body.count; ++i)
		{
			if (i > 0)
				mn::print_to(self.out, " else ");

			mn::print_to(self.out, "if (");
			hlsl_expr_gen(self, s->if_stmt.cond[i]);
			mn::print_to(self.out, ") ");
			_hlsl_gen_block_stmt(self, s->if_stmt.body[i]);
		}

		if (s->if_stmt.else_body != nullptr)
		{
			mn::print_to(self.out, " else ");
			_hlsl_gen_block_stmt(self, s->if_stmt.else_body);
		}
	}

	inline static void
	_hlsl_gen_for_stmt(HLSL& self, Stmt* s)
	{
		_hlsl_enter_scope(self, unit_scope_find(self.unit->parent_unit, s));
		mn_defer{_hlsl_leave_scope(self);};

		mn::print_to(self.out, "{{ // for scope");
		++self.indent;

		_hlsl_newline(self);
		mn::print_to(self.out, "// for init statement");
		if (s->for_stmt.init != nullptr)
		{
			_hlsl_newline(self);
			hlsl_stmt_gen(self, s->for_stmt.init);
			if (_hlsl_add_semicolon_after(self, s->for_stmt.init))
				mn::print_to(self.out, ";");
		}

		_hlsl_newline(self);
		mn::print_to(self.out, "[loop]");
		_hlsl_newline(self);
		mn::print_to(self.out, "while (");
		if (s->for_stmt.cond != nullptr)
			hlsl_expr_gen(self, s->for_stmt.cond);
		else
			mn::print_to(self.out, "true");
		mn::print_to(self.out, ") {{");
		++self.indent;

		_hlsl_newline(self);
		mn::print_to(self.out, "// for body");
		_hlsl_loop_post_stmt_enter(self, s->for_stmt.post);
		for (auto stmt: s->for_stmt.body->block_stmt)
		{
			_hlsl_newline(self);
			hlsl_stmt_gen(self, stmt);
			if (_hlsl_add_semicolon_after(self, stmt))
			{
				mn::print_to(self.out, ";");
			}
		}
		_hlsl_loop_post_stmt_leave(self);

		if (s->for_stmt.post != nullptr)
		{
			_hlsl_newline(self);
			mn::print_to(self.out, "// for post statement");
			_hlsl_newline(self);
			hlsl_stmt_gen(self, s->for_stmt.post);
			mn::print_to(self.out, ";");
		}

		--self.indent;
		_hlsl_newline(self);
		mn::print_to(self.out, "}}");

		--self.indent;
		_hlsl_newline(self);
		mn::print_to(self.out, "}} // for scope");
	}

	inline static void
	_hlsl_assign(HLSL& self, Expr* lhs, const Tkn& op, Expr* rhs)
	{
		if (auto it = mn::map_lookup(self.buffer_access_info, lhs); it && it->value.is_write)
		{
			auto& info = it->value;
			mn_assert(info.size <= 16);

			const char* load_name = nullptr;
			if (op.kind != Tkn::KIND_EQUAL)
				load_name = _hlsl_gen_compute_buffer_load(self, nullptr, info, lhs->type);

			// ignore such thing
			_hlsl_gen_compute_buffer_store(self, info, load_name, op.kind, rhs, rhs->type);
		}
		else
		{
			hlsl_expr_gen(self, lhs);
			mn::print_to(self.out, " {} ", op.str);
			hlsl_expr_gen(self, rhs);
		}
	}

	inline static void
	_hlsl_assign(HLSL& self, Symbol* lhs, const Tkn& op, Expr* rhs)
	{
		auto e = expr_atom_new(self.unit->symbols_arena, lhs->var_sym.name);
		e->type = lhs->type;
		e->mode = ADDRESS_MODE_VARIABLE;
		_hlsl_assign(self, e, op, rhs);
	}

	inline static void
	_hlsl_gen_assign_stmt(HLSL& self, Stmt* s)
	{
		for (size_t i = 0; i < s->assign_stmt.lhs.count; ++i)
		{
			if (i > 0)
			{
				mn::print_to(self.out, ";");
				_hlsl_newline(self);
			}

			auto lhs = s->assign_stmt.lhs[i];
			auto rhs = s->assign_stmt.rhs[i];

			_hlsl_assign(self, lhs, s->assign_stmt.op, rhs);
		}
	}

	inline static void
	_hlsl_symbol_gen(HLSL& self, Symbol* sym, bool in_stmt);

	inline static void
	_hlsl_gen_decl_stmt(HLSL& self, Stmt* s)
	{
		auto scope = _hlsl_current_scope(self);
		auto d = s->decl_stmt;
		switch (d->kind)
		{
		case Decl::KIND_VAR:
		{
			for (size_t i = 0; i < d->var_decl.names.count; ++i)
			{
				if (i > 0)
					_hlsl_newline(self);
				auto name = d->var_decl.names[i];
				_hlsl_symbol_gen(self, scope_find(scope, name.str), true);
			}
			break;
		}
		case Decl::KIND_CONST:
		{
			for (size_t i = 0; i < d->const_decl.names.count; ++i)
			{
				if (i > 0)
					_hlsl_newline(self);
				auto name = d->const_decl.names[i];
				_hlsl_symbol_gen(self, scope_find(scope, name.str), true);
			}
			break;
		}
		case Decl::KIND_FUNC:
			// internal/private functions (functions in functions) are generated on their own, here we ignore it
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_func_gen_internal(HLSL& self, Decl* d, Type* t, const char* name)
	{
		// don't generate templated functions
		if (type_is_templated(t))
			return;

		bool is_builtin = false;

		if (mn::map_lookup(d->tags.table, KEYWORD_BUILTIN))
			is_builtin = true;

		if (is_builtin)
			return;

		auto return_type = t->as_func.sign.return_type;
		mn::print_to(self.out, "{} {}(", _hlsl_write_field(self, return_type, ""), _hlsl_name(self, name));

		if (d->func_decl.body != nullptr)
			_hlsl_enter_scope(self, unit_scope_find(self.unit->parent_unit, d));
		mn_defer{if (d->func_decl.body) _hlsl_leave_scope(self);};

		size_t i = 0;
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = t->as_func.sign.args.types[i];
			for (auto name: arg.names)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");

				if (mn::map_lookup(arg.tags.table, KEYWORD_POINT))
					mn::print_to(self.out, "point ");
				else if (mn::map_lookup(arg.tags.table, KEYWORD_LINE))
					mn::print_to(self.out, "line ");
				else if (mn::map_lookup(arg.tags.table, KEYWORD_TRIANGLE))
					mn::print_to(self.out, "triangle ");

				if (mn::map_lookup(arg.tags.table, KEYWORD_IN))
					mn::print_to(self.out, "in ");
				else if (mn::map_lookup(arg.tags.table, KEYWORD_OUT))
					mn::print_to(self.out, "out ");
				else if (mn::map_lookup(arg.tags.table, KEYWORD_INOUT))
					mn::print_to(self.out, "inout ");

				mn::print_to(self.out, "{}", _hlsl_write_field(self, arg_type, name.str));
				++i;
			}
		}
		mn::print_to(self.out, ")");

		if (d->func_decl.body != nullptr)
		{
			mn::print_to(self.out, " ");
			_hlsl_gen_block_stmt(self, d->func_decl.body);
		}
	}

	inline static void
	_hlsl_func_gen(HLSL& self, Symbol* sym)
	{
		_hlsl_func_gen_internal(self, sym->func_sym.decl, sym->type, _hlsl_symbol_name(self, sym));
	}

	inline static void
	_hlsl_func_instantiation_gen(HLSL& self, Symbol* sym)
	{
		_hlsl_func_gen_internal(self, sym->as_func_instantiation.decl, sym->type, _hlsl_symbol_name(self, sym));
	}

	inline static void
	_hlsl_var_gen(HLSL& self, Symbol* sym, bool in_stmt)
	{
		if (sym->var_sym.value && in_stmt == false)
			_hlsl_rewrite_complits_in_expr(self, sym->var_sym.value, false);

		if (sym->var_sym.is_uniform)
		{
			auto uniform_name = _hlsl_name(self, _hlsl_symbol_name(self, sym));
			auto uniform_block_name = uniform_name;
			if (sym->type->kind != Type::KIND_STRUCT)
			{
				uniform_block_name = _hlsl_name(self, mn::str_tmpf("uniform{}", _hlsl_tmp_name(self)).ptr);
			}

			if (sym->type->kind == Type::KIND_TEXTURE)
			{
				mn::print_to(self.out, "{}: register(t{})", _hlsl_write_field(self, sym->type, uniform_name), sym->var_sym.uniform_binding);
			}
			else if (type_is_sampler(sym->type))
			{
				mn::print_to(self.out, "{}: register(s{})", _hlsl_write_field(self, sym->type, uniform_name), sym->var_sym.uniform_binding);
			}
			else
			{
				mn::print_to(self.out, "cbuffer {}: register(b{}) {{", uniform_block_name, sym->var_sym.uniform_binding);
				++self.indent;
				{
					auto type = sym->type;
					if (type->kind == Type::KIND_STRUCT)
					{
						for (auto field: type->struct_type.fields)
						{
							_hlsl_newline(self);
							auto name = mn::str_tmpf("{}_{}", uniform_name, field.name.str);
							mn::print_to(self.out, "{}", _hlsl_write_field(self, field.type, name.ptr));

							auto whole_part = field.aligned_offset / 16;
							auto remainder_part = field.aligned_offset % 16;
							if (remainder_part == 0)
							{
								mn::print_to(self.out, ": packoffset(c{})", whole_part);
							}
							else if (remainder_part == 12)
							{
								mn::print_to(self.out, ": packoffset(c{}.w)", whole_part);
							}
							else if (remainder_part == 8)
							{
								mn::print_to(self.out, ": packoffset(c{}.z)", whole_part);
							}
							else if (remainder_part == 4)
							{
								mn::print_to(self.out, ": packoffset(c{}.y)", whole_part);
							}
							else
							{
								mn_unreachable();
							}
							mn::print_to(self.out, ";");
						}
					}
					else
					{
						_hlsl_newline(self);
						mn::print_to(self.out, "{};", _hlsl_write_field(self, type, uniform_name));
					}
				}
				--self.indent;
				_hlsl_newline(self);
				mn::print_to(self.out, "}}");
			}
		}
		else if (sym->var_sym.is_buffer)
		{
			auto buffer_name = _hlsl_name(self, _hlsl_symbol_name(self, sym));
			if (sym->var_sym.is_read_write)
				mn::print_to(self.out, "RW");
			mn::print_to(self.out, "ByteAddressBuffer {}", buffer_name);
		}
		else
		{
			mn::print_to(self.out, "{}", _hlsl_write_field(self, sym->type, _hlsl_symbol_name(self, sym)));
			if (sym->var_sym.value != nullptr)
			{
				mn::print_to(self.out, " = ");
				hlsl_expr_gen(self, sym->var_sym.value);
			}
			else
			{
				// TODO(Moustapha): handle zero init data
			}
		}
		mn::print_to(self.out, ";");
	}

	inline static void
	_hlsl_const_gen(HLSL& self, Symbol* sym, bool in_stmt)
	{
		if (sym->const_sym.value && in_stmt == false)
			_hlsl_rewrite_complits_in_expr(self, sym->const_sym.value, true);

		mn::print_to(self.out, "static const {}", _hlsl_write_field(self, sym->type, _hlsl_symbol_name(self, sym)));
		if (sym->const_sym.value != nullptr)
		{
			mn::print_to(self.out, " = ");
			hlsl_expr_gen(self, sym->var_sym.value);
		}
		else
		{
			mn::print_to(self.out, " = ");
			_hlsl_zero_value(self, sym->type);
		}
		mn::print_to(self.out, ";");
	}

	inline static void
	_hlsl_struct_gen_internal(HLSL& self, Symbol* sym, Decl* decl)
	{
		// ignore builtin structs
		if (mn::map_lookup(decl->tags.table, KEYWORD_BUILTIN))
			return;

		mn::print_to(self.out, "struct {} {{", _hlsl_name(self, _hlsl_symbol_name(self, sym)));
		++self.indent;

		auto t = sym->type;

		auto io_flags_it = mn::map_lookup(self.io_structs, sym);

		size_t i = 0;
		for (auto field: decl->struct_decl.fields)
		{
			auto field_type = t->struct_type.fields[i];
			for (auto name: field.names)
			{
				_hlsl_newline(self);
				mn::print_to(self.out, "{}", _hlsl_write_field(self, field_type.type, name.str));
				if (io_flags_it)
				{
					if (mn::map_lookup(field.tags.table, KEYWORD_SV_POSITION) != nullptr)
					{
						mn::print_to(self.out, ": SV_POSITION");
					}
					else if (mn::map_lookup(field.tags.table, KEYWORD_SV_DEPTH) != nullptr)
					{
						mn::print_to(self.out, ": SV_DEPTH");
					}
					else if (mn::map_lookup(field.tags.table, KEYWORD_SV_PRIMITIVE_ID) != nullptr)
					{
						mn::print_to(self.out, ": SV_PrimitiveID");
					}
					else if (mn::map_lookup(field.tags.table, KEYWORD_SV_THREAD_ID) != nullptr)
					{
						mn::print_to(self.out, ": SV_DispatchThreadID");
					}
					else if (io_flags_it->value == ENTRY_IO_FLAG_PIXEL_OUT)
					{
						mn::print_to(self.out, ": SV_TARGET{}", i);
					}
					else
					{
						mn::print_to(self.out, ": TEXCOORD{}", i);
					}
				}
				mn::print_to(self.out, ";");
				++i;
			}
		}

		--self.indent;
		_hlsl_newline(self);
		mn::print_to(self.out, "}};");
	}

	inline static void
	_hlsl_struct_gen(HLSL& self, Symbol* sym)
	{
		// don't generate templated structs
		if (type_is_templated(sym->type))
			return;

		_hlsl_struct_gen_internal(self, sym, sym->struct_sym.decl);
	}

	inline static void
	_hlsl_struct_instantiation_gen(HLSL& self, Symbol* sym)
	{
		// don't generate templated structs
		if (type_is_templated(sym->type))
			return;

		_hlsl_struct_gen_internal(self, sym, sym->as_struct_instantiation.template_symbol->struct_sym.decl);
	}

	inline static void
	_hlsl_enum_gen(HLSL& self, Symbol* sym)
	{
		// glslang hlsl implementation doesn't work with enums so for now we generate them as macros an integers
		mn::print_to(self.out, "#define {} int", _hlsl_symbol_name(self, sym));

		auto d = sym->enum_sym.decl;
		auto t = sym->type;

		size_t i = 0;
		for (auto field: d->enum_decl.fields)
		{
			auto field_type = t->enum_type.fields[i];
			mn_assert(field_type.value.type == type_int);
			_hlsl_newline(self);
			mn::print_to(self.out, "#define {}_{} {}", _hlsl_symbol_name(self, sym), field.name.str, field_type.value.as_int);
			++i;
		}

		// mn::print_to(self.out, "enum {} {{", _hlsl_name(self, _hlsl_symbol_name(self, sym)));
		// ++self.indent;

		// auto d = sym->enum_sym.decl;
		// auto t = sym->type;

		// size_t i = 0;
		// for (auto field: d->enum_decl.fields)
		// {
		// 	if (i > 0)
		// 		mn::print_to(self.out, ", ");
		// 	auto field_type = t->enum_type.fields[i];
		// 	mn_assert(field_type.value.type == type_int);
		// 	_hlsl_newline(self);
		// 	mn::print_to(self.out, "{}_{} = {}", _hlsl_symbol_name(self, sym), field.name.str, field_type.value.as_int);
		// 	++i;
		// }

		// --self.indent;
		// _hlsl_newline(self);
		// mn::print_to(self.out, "}};");
	}

	inline static void
	_hlsl_symbol_gen(HLSL& self, Symbol* sym, bool in_stmt)
	{
		switch (sym->kind)
		{
		case Symbol::KIND_FUNC:
			_hlsl_func_gen(self, sym);
			break;
		case Symbol::KIND_VAR:
			_hlsl_var_gen(self, sym, in_stmt);
			break;
		case Symbol::KIND_CONST:
			_hlsl_const_gen(self, sym, in_stmt);
			break;
		case Symbol::KIND_FUNC_OVERLOAD_SET:
		{
			bool was_last_symbol_generated = false;
			for (size_t i = 0; i < sym->func_overload_set_sym.used_decls.count; ++i)
			{
				auto decl = sym->func_overload_set_sym.used_decls[i];
				auto it = mn::map_lookup(sym->func_overload_set_sym.decls, decl);
				auto type = it->value;

				if (was_last_symbol_generated)
					_hlsl_newline(self);

				auto pos = _hlsl_buffer_position(self);
				_hlsl_func_gen_internal(self, decl, type, _hlsl_symbol_name(self, sym, decl));
				was_last_symbol_generated = _hlsl_code_generated_after(self, pos);
			}
			break;
		}
		case Symbol::KIND_STRUCT:
			_hlsl_struct_gen(self, sym);
			break;
		case Symbol::KIND_PACKAGE:
		{
			if (self.entry == nullptr)
			{
				auto package = sym->package_sym.package;
				if (package->stage == COMPILATION_STAGE_CODEGEN)
				{
					for (size_t i = 0; i < package->reachable_symbols.count; ++i)
					{
						if (i > 0)
							_hlsl_newline(self);
						_hlsl_symbol_gen(self, package->reachable_symbols[i], in_stmt);
					}
					if (package->reachable_symbols.count > 0)
						_hlsl_newline(self);
					package->stage = COMPILATION_STAGE_SUCCESS;
				}
			}
			break;
		}
		case Symbol::KIND_ENUM:
			_hlsl_enum_gen(self, sym);
			break;
		case Symbol::KIND_STRUCT_INSTANTIATION:
			_hlsl_struct_instantiation_gen(self, sym);
			break;
		case Symbol::KIND_FUNC_INSTANTIATION:
			_hlsl_func_instantiation_gen(self, sym);
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	inline static void
	_hlsl_generate_vertex_shader_io(HLSL& self, Symbol* entry)
	{
		auto decl = entry->func_sym.decl;
		auto entry_type = entry->type;
		size_t type_index = 0;
		// generate input
		for (size_t i = 0; i < decl->func_decl.args.count; ++i)
		{
			const auto& arg = decl->func_decl.args[i];

			for (const auto& name: arg.names)
			{
				auto input_name = _hlsl_name(self, name.str);
				auto arg_type = entry_type->as_func.sign.args.types[type_index++];
				switch(arg_type->kind)
				{
				case Type::KIND_STRUCT:
					mn::map_insert(self.io_structs, arg_type->struct_type.symbol, ENTRY_IO_FLAG_NONE);
					break;
				default:
					mn_unreachable();
					break;
				}
			}
		}

		size_t out_location = 0;
		if (entry_type->as_func.sign.return_type != type_void)
		{
			auto ret_type = entry_type->as_func.sign.return_type;
			if (auto sym = type_symbol(ret_type))
				mn::map_insert(self.io_structs, sym, ENTRY_IO_FLAG_NONE);
		}

		if (out_location > 0)
			_hlsl_newline(self);
	}

	inline static void
	_hlsl_generate_pixel_shader_io(HLSL& self, Symbol* entry)
	{
		auto decl = entry->func_sym.decl;
		auto entry_type = entry->type;
		size_t type_index = 0;
		// generate input
		for (size_t i = 0; i < decl->func_decl.args.count; ++i)
		{
			const auto& arg = decl->func_decl.args[i];

			for (const auto& name: arg.names)
			{
				auto input_name = _hlsl_name(self, name.str);
				auto arg_type = entry_type->as_func.sign.args.types[type_index++];
				switch(arg_type->kind)
				{
				case Type::KIND_STRUCT:
					mn::map_insert(self.io_structs, arg_type->struct_type.symbol, ENTRY_IO_FLAG_NONE);
					break;
				default:
					mn_unreachable();
					break;
				}
			}
		}

		size_t out_location = 0;
		if (entry_type->as_func.sign.return_type != type_void)
		{
			auto ret_type = entry_type->as_func.sign.return_type;
			if (auto sym = type_symbol(ret_type))
				mn::map_insert(self.io_structs, sym, ENTRY_IO_FLAG_PIXEL_OUT);
		}

		if (out_location > 0)
			_hlsl_newline(self);
	}

	inline static void
	_hlsl_generate_geometry_shader_io(HLSL& self, Symbol* entry)
	{
		auto decl = entry->func_sym.decl;
		auto entry_type = entry->type;

		// extract geometry shader data
		// generate name for geometry shader output stream
		self.geometry_stream_name = _hlsl_tmp_name(self);

		size_t type_index = 0;
		// generate input
		for (size_t i = 0; i < decl->func_decl.args.count; ++i)
		{
			const auto& arg = decl->func_decl.args[i];

			for (const auto& name: arg.names)
			{
				auto input_name = _hlsl_name(self, name.str);
				auto arg_type = entry_type->as_func.sign.args.types[type_index++];
				switch(arg_type->kind)
				{
				case Type::KIND_STRUCT:
					mn::map_insert(self.io_structs, arg_type->struct_type.symbol, ENTRY_IO_FLAG_NONE);
					break;
				case Type::KIND_ARRAY:
					if (auto sym = type_symbol(arg_type->array.base))
						mn::map_insert(self.io_structs, sym, ENTRY_IO_FLAG_NONE);
					break;
				case Type::KIND_TRIANGLE_STREAM:
				case Type::KIND_LINE_STREAM:
				case Type::KIND_POINT_STREAM:
					for (auto template_arg: arg_type->template_base_args)
						if (auto sym = type_symbol(template_arg))
							mn::map_insert(self.io_structs, sym, ENTRY_IO_FLAG_NONE);
					// do nothing
					break;
				default:
					mn_unreachable();
					break;
				}
			}
		}

		size_t out_location = 0;
		if (entry_type->as_func.sign.return_type != type_void)
		{
			auto ret_type = entry_type->as_func.sign.return_type;
			if (auto sym = type_symbol(ret_type))
				mn::map_insert(self.io_structs, sym, ENTRY_IO_FLAG_NONE);
		}

		if (out_location > 0)
			_hlsl_newline(self);
	}

	inline static void
	_hlsl_generate_compute_shader_io(HLSL& self, Symbol* entry)
	{
		auto decl = entry->func_sym.decl;
		auto entry_type = entry->type;

		size_t type_index = 0;
		// generate input
		for (size_t i = 0; i < decl->func_decl.args.count; ++i)
		{
			const auto& arg = decl->func_decl.args[i];

			for (const auto& name: arg.names)
			{
				auto input_name = _hlsl_name(self, name.str);
				auto arg_type = entry_type->as_func.sign.args.types[type_index++];
				switch(arg_type->kind)
				{
				case Type::KIND_STRUCT:
					mn::map_insert(self.io_structs, arg_type->struct_type.symbol, ENTRY_IO_FLAG_NONE);
					break;
				case Type::KIND_ARRAY:
					if (auto sym = type_symbol(arg_type->array.base))
						mn::map_insert(self.io_structs, sym, ENTRY_IO_FLAG_NONE);
					break;
				case Type::KIND_VEC:
					// do nothing
					break;
				default:
					mn_unreachable();
					break;
				}
			}
		}

		size_t out_location = 0;
		if (entry_type->as_func.sign.return_type != type_void)
		{
			auto ret_type = entry_type->as_func.sign.return_type;
			if (auto sym = type_symbol(ret_type))
				mn::map_insert(self.io_structs, sym, ENTRY_IO_FLAG_NONE);
		}

		if (out_location > 0)
			_hlsl_newline(self);
	}

	inline static void
	_hlsl_generate_main_func(HLSL& self, Symbol* entry)
	{
		mn_assert(entry->kind == Symbol::KIND_FUNC);

		auto d = entry->func_sym.decl;
		auto t = entry->type;
		auto return_type = t->as_func.sign.return_type;
		if (auto geometry_tag = mn::map_lookup(d->tags.table, KEYWORD_GEOMETRY))
		{
			auto geometry_max_vertex_count = mn::map_lookup(geometry_tag->value.args, KEYWORD_MAX_VERTEX_COUNT);
			auto const_value = geometry_max_vertex_count->value.value->const_value;
			mn_assert(const_value.type == type_int);
			mn::print_to(self.out, "[maxvertexcount({})]", const_value.as_int);
			_hlsl_newline(self);
		}
		else if (auto compute_tag = mn::map_lookup(d->tags.table, KEYWORD_COMPUTE))
		{
			int x = 1, y = 1, z = 1;
			if (auto compute_x = mn::map_lookup(compute_tag->value.args, KEYWORD_X))
			{
				auto const_x = compute_x->value.value->const_value;
				mn_assert(const_x.type == type_int);
				x = const_x.as_int;
			}

			if (auto compute_y = mn::map_lookup(compute_tag->value.args, KEYWORD_Y))
			{
				auto const_y = compute_y->value.value->const_value;
				mn_assert(const_y.type == type_int);
				y = const_y.as_int;
			}

			if (auto compute_z = mn::map_lookup(compute_tag->value.args, KEYWORD_Z))
			{
				auto const_z = compute_z->value.value->const_value;
				mn_assert(const_z.type == type_int);
				z = const_z.as_int;
			}

			mn::print_to(self.out, "[numthreads({}, {}, {})]", x, y, z);
			_hlsl_newline(self);
		}

		mn::print_to(self.out, "{} main(", _hlsl_write_field(self, return_type, ""));

		if (d->func_decl.body != nullptr)
			_hlsl_enter_scope(self, unit_scope_find(self.unit->parent_unit, d));
		mn_defer{if (d->func_decl.body) _hlsl_leave_scope(self);};

		size_t i = 0;
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = t->as_func.sign.args.types[i];
			for (auto name: arg.names)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");

				if (mn::map_lookup(arg.tags.table, KEYWORD_POINT))
					mn::print_to(self.out, "point ");
				else if (mn::map_lookup(arg.tags.table, KEYWORD_LINE))
					mn::print_to(self.out, "line ");
				else if (mn::map_lookup(arg.tags.table, KEYWORD_TRIANGLE))
					mn::print_to(self.out, "triangle ");

				if (mn::map_lookup(arg.tags.table, KEYWORD_IN))
					mn::print_to(self.out, "in ");
				else if (mn::map_lookup(arg.tags.table, KEYWORD_OUT))
					mn::print_to(self.out, "out ");
				else if (mn::map_lookup(arg.tags.table, KEYWORD_INOUT))
					mn::print_to(self.out, "inout ");

				mn::print_to(self.out, "{}", _hlsl_write_field(self, arg_type, name.str));

				if (mn::map_lookup(arg.tags.table, KEYWORD_SV_THREAD_ID))
					mn::print_to(self.out, ": SV_DispatchThreadID");
				++i;
			}
		}
		mn::print_to(self.out, ")");

		_hlsl_newline(self);
		mn::print_to(self.out, "{{");
		++self.indent;

		_hlsl_newline(self);
		if (return_type && return_type != type_void)
		{
			mn::print_to(self.out, "return ");
		}

		mn::print_to(self.out, "{}(", _hlsl_name(self, _hlsl_symbol_name(self, entry)));
		i = 0;
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = t->as_func.sign.args.types[i];
			for (auto name: arg.names)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				mn::print_to(self.out, "{}", name.str);
				++i;
			}
		}
		mn::print_to(self.out, ");");

		--self.indent;
		_hlsl_newline(self);
		mn::print_to(self.out, "}}");
	}


	// API
	HLSL
	hlsl_new(Unit_Package* unit, mn::Stream out)
	{
		HLSL self{};
		self.unit = unit;
		self.out = out;
		self.arena = mn::allocator_arena_new();
		self.scope_stack = mn::buf_with_allocator<Scope*>(self.arena);
		self.loop_post_stmt_stack = mn::buf_with_allocator<Stmt*>(self.arena);
		self.reserved_to_alternative = mn::map_with_allocator<const char*, const char*>(self.arena);
		self.symbol_to_names = mn::map_with_allocator<void*, const char*>(self.arena);
		self.io_structs = mn::map_with_allocator<Symbol*, ENTRY_IO_FLAG>(self.arena);
		self.template_mangled_names = mn::map_with_allocator<Type*, const char*>(self.arena);
		self.buffer_access_info = mn::map_with_allocator<Expr*, Buffer_Access_Info>(self.arena);

		// push global scope as first entry in scope stack
		auto global_scope = self.unit->global_scope;
		mn_assert(global_scope != nullptr);
		mn::buf_push(self.scope_stack, global_scope);

		constexpr auto keywords_count = sizeof(HLSL_KEYWORDS) / sizeof(*HLSL_KEYWORDS);
		for (size_t i = 0; i < keywords_count; ++i)
		{
			auto keyword = unit_intern(self.unit->parent_unit, HLSL_KEYWORDS[i]);
			mn::map_insert(self.reserved_to_alternative, keyword, (const char*)nullptr);
		}

		return self;
	}

	void
	hlsl_free(HLSL& self)
	{
		mn::allocator_free(self.arena);
	}

	void
	hlsl_expr_gen(HLSL& self, Expr* e)
	{
		if (e->in_parens)
			mn::print_to(self.out, "(");

		switch (e->kind)
		{
		case Expr::KIND_ATOM:
			_hlsl_gen_atom_expr(self, e);
			break;
		case Expr::KIND_BINARY:
			_hlsl_gen_binary_expr(self, e);
			break;
		case Expr::KIND_UNARY:
			_hlsl_gen_unary_expr(self, e);
			break;
		case Expr::KIND_DOT:
			_hlsl_gen_dot_expr(self, e);
			break;
		case Expr::KIND_INDEXED:
			_hlsl_gen_indexed_expr(self, e);
			break;
		case Expr::KIND_CALL:
			_hlsl_gen_call_expr(self, e);
			break;
		case Expr::KIND_CAST:
			_hlsl_gen_cast_expr(self, e);
			break;
		case Expr::KIND_COMPLIT:
			_hlsl_gen_complit_expr(self, e);
			break;
		default:
			mn_unreachable();
			break;
		}

		if (e->in_parens)
			mn::print_to(self.out, ")");
	}

	void
	hlsl_stmt_gen(HLSL& self, Stmt* s)
	{
		// handle compound literal expressions
		bool is_const = s->kind == Stmt::KIND_DECL && s->decl_stmt->kind == Decl::KIND_CONST;
		_hlsl_rewrite_complits_in_stmt(self, s, is_const);

		_hlsl_rewrite_buffer_access_in_stmt(self, s);
		_hlsl_rewrite_buffer_access_in_stmt_gen(self, s);

		switch (s->kind)
		{
		case Stmt::KIND_BREAK:
			_hlsl_gen_break_stmt(self, s);
			break;
		case Stmt::KIND_CONTINUE:
			_hlsl_gen_continue_stmt(self, s);
			break;
		case Stmt::KIND_DISCARD:
			_hlsl_gen_discard_stmt(self, s);
			break;
		case Stmt::KIND_RETURN:
			_hlsl_gen_return_stmt(self, s);
			break;
		case Stmt::KIND_IF:
			_hlsl_gen_if_stmt(self, s);
			break;
		case Stmt::KIND_FOR:
			_hlsl_gen_for_stmt(self, s);
			break;
		case Stmt::KIND_ASSIGN:
			_hlsl_gen_assign_stmt(self, s);
			break;
		case Stmt::KIND_EXPR:
			hlsl_expr_gen(self, s->expr_stmt);
			break;
		case Stmt::KIND_BLOCK:
			_hlsl_gen_block_stmt(self, s);
			break;
		case Stmt::KIND_DECL:
			_hlsl_gen_decl_stmt(self, s);
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	void
	hlsl_gen_entry(HLSL& self, Entry_Point* entry)
	{
		entry_point_calc_reachable_list(entry);

		self.entry = entry;

		switch (entry->mode)
		{
		case COMPILATION_MODE_VERTEX:
			_hlsl_generate_vertex_shader_io(self, entry->symbol);
			break;
		case COMPILATION_MODE_PIXEL:
			_hlsl_generate_pixel_shader_io(self, entry->symbol);
			break;
		case COMPILATION_MODE_GEOMETRY:
			_hlsl_generate_geometry_shader_io(self, entry->symbol);
			break;
		case COMPILATION_MODE_COMPUTE:
			_hlsl_generate_compute_shader_io(self, entry->symbol);
			break;
		case COMPILATION_MODE_LIBRARY:
			// library mode is not allowed
		default:
			mn_unreachable();
			break;
		}

		// now that we have our dependencies ordered we'll just traverse them back to front and generate them
		bool last_symbol_was_generated = false;
		for (auto sym: entry->reachable_symbols)
		{
			if (last_symbol_was_generated)
				_hlsl_newline(self);

			auto pos = _hlsl_buffer_position(self);
			_hlsl_symbol_gen(self, sym, false);
			last_symbol_was_generated = _hlsl_code_generated_after(self, pos);
		}

		// generate real entry function
		_hlsl_newline(self);
		_hlsl_newline(self);
		_hlsl_generate_main_func(self, entry->symbol);
	}

	void
	hlsl_gen_library(HLSL& self)
	{
		self.entry = nullptr;

		bool last_symbol_was_generated = false;
		for (size_t i = 0; i < self.unit->reachable_symbols.count; ++i)
		{
			if (last_symbol_was_generated)
				_hlsl_newline(self);

			auto pos = _hlsl_buffer_position(self);
			_hlsl_symbol_gen(self, self.unit->reachable_symbols[i], false);
			last_symbol_was_generated = _hlsl_code_generated_after(self, pos);
		}
	}
}