#include "sabre/HLSL.h"
#include "sabre/AST.h"
#include "sabre/Type_Interner.h"
#include "sabre/Unit.h"

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

	inline static const char*
	_hlsl_symbol_name(Symbol* sym, Decl* decl = nullptr)
	{
		bool use_raw_name = false;

		const char* res = sym->package_name;
		switch (sym->kind)
		{
		// in case the function is a builtin function we don't use it's package name
		case Symbol::KIND_FUNC:
			if (auto tag = mn::map_lookup(sym->func_sym.decl->tags.table, KEYWORD_BUILTIN))
			{
				if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_GLSL))
				{
					res = arg->value.value.str;
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
					if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_GLSL))
					{
						res = arg->value.value.str;
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
						if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_GLSL))
						{
							res = arg->value.value.str;
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
					assert(false && "unreachable");
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
					assert(false && "unreachable");
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
					assert(false && "unreachable");
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
					assert(false && "unreachable");
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
					assert(false && "unreachable");
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
				assert(false && "unreachable");
			}
			break;
		case Type::KIND_STRUCT:
			can_write_name = true;
			str = mn::strf(str, "{}", _hlsl_name(self, _hlsl_symbol_name(type->struct_type.symbol)));
			break;
		case Type::KIND_TEXTURE:
			can_write_name = true;
			if (type == type_texture1d)
			{
				str = mn::strf(str, "Texture1D");
			}
			else if (type == type_texture2d)
			{
				str = mn::strf(str, "Texture2D");
			}
			else if (type == type_texture3d)
			{
				str = mn::strf(str, "Texture3D");
			}
			else if (type == type_texture_cube)
			{
				str = mn::strf(str, "TextureCube");
			}
			else
			{
				assert(false && "unreachable");
			}
			break;
		case Type::KIND_ARRAY:
		{
			auto str_name = mn::str_lit(name);
			auto array_name = mn::str_tmpf("{}[{}]", str_name, type->array.count);
			str = _hlsl_write_field(self, str, type->array.base, array_name.ptr);
			break;
		}
		case Type::KIND_ENUM:
			can_write_name = true;
			str = mn::strf(str, "{}", _hlsl_name(self, _hlsl_symbol_name(type->enum_type.symbol)));
			break;
		default:
			assert(false && "unreachable");
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
	_hlsl_gen_atom_expr(HLSL& self, Expr* e)
	{
		if (e->atom.tkn.kind == Tkn::KIND_ID)
		{
			if (e->atom.sym)
			{
				auto package_name = mn::str_lit(_hlsl_symbol_name(e->atom.sym, e->atom.decl));
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

	inline static void
	_hlsl_gen_unary_expr(HLSL& self, Expr* e)
	{
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
			is_lhs_package = lhs->atom.sym->kind == Symbol::KIND_PACKAGE;

		if (is_lhs_package)
		{
			hlsl_expr_gen(self, e->dot.rhs);
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
		hlsl_expr_gen(self, e->indexed.base);
		mn::print_to(self.out, "[");
		hlsl_expr_gen(self, e->indexed.index);
		mn::print_to(self.out, "]");
	}

	inline static void
	_hlsl_gen_call_expr(HLSL& self, Expr* e)
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
			assert(false && "unreachable");
		}
	}


	// API
	HLSL
	hlsl_new(Unit_Package* unit, mn::Stream out)
	{
		HLSL self{};
		self.unit = unit;
		self.out = out;

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
		mn::map_free(self.reserved_to_alternative);
		mn::map_free(self.symbol_to_names);
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
			assert(false && "unreachable");
			break;
		}

		if (e->in_parens)
			mn::print_to(self.out, ")");
	}
}