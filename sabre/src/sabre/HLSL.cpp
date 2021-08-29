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
		assert(scope != nullptr);
		mn::buf_push(self.scope_stack, scope);
	}

	inline static void
	_hlsl_leave_scope(HLSL& self)
	{
		assert(self.scope_stack.count > 1);
		mn::buf_pop(self.scope_stack);
	}

	inline static Scope*
	_hlsl_current_scope(HLSL& self)
	{
		return mn::buf_top(self.scope_stack);
	}

	inline static Symbol*
	_hlsl_find_symbol(HLSL& self, const char* name)
	{
		auto current_scope = _hlsl_current_scope(self);
		return scope_find(current_scope, name);
	}

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
				if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_HLSL))
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
					if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_HLSL))
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
						if (auto arg = mn::map_lookup(tag->value.args, KEYWORD_HLSL))
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
		case Type::KIND_SAMPLER:
			can_write_name = true;
			str = mn::strf(str, "SamplerState");
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
			assert(false && "unreachable");
			break;
		}
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
		else if (is_lhs_enum)
		{
			auto enum_name = _hlsl_symbol_name(e->type->enum_type.symbol);
			mn::print_to(self.out, "{0}::{0}_", enum_name);
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
			assert(false && "unreachable");
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
			mn::print_to(self.out, "const ");

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
			assert(false && "unreachable");
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
			assert(false && "unreachable");
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
		default:
			assert(false && "unreachable");
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
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_hlsl_gen_break_stmt(HLSL& self, Stmt* s)
	{
		mn::print_to(self.out, "break");
	}

	inline static void
	_hlsl_gen_continue_stmt(HLSL& self, Stmt* s)
	{
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
		mn_defer(_hlsl_leave_scope(self));

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
		mn::print_to(self.out, "while (");
		if (s->for_stmt.cond != nullptr)
			hlsl_expr_gen(self, s->for_stmt.cond);
		else
			mn::print_to(self.out, "true");
		mn::print_to(self.out, ") {{");
		++self.indent;

		_hlsl_newline(self);
		mn::print_to(self.out, "// for body");
		for (auto stmt: s->for_stmt.body->block_stmt)
		{
			_hlsl_newline(self);
			hlsl_stmt_gen(self, stmt);
			if (_hlsl_add_semicolon_after(self, stmt))
			{
				mn::print_to(self.out, ";");
			}
		}

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
	_hlsl_assign(HLSL& self, Expr* lhs, const char* op, Expr* rhs)
	{
		hlsl_expr_gen(self, lhs);
		mn::print_to(self.out, " {} ", op);
		hlsl_expr_gen(self, rhs);
	}

	inline static void
	_hlsl_assign(HLSL& self, Symbol* lhs, const char* op, Expr* rhs)
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

			_hlsl_assign(self, lhs, s->assign_stmt.op.str, rhs);
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
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_hlsl_func_gen_internal(HLSL& self, Decl* d, Type* t, const char* name)
	{
		bool is_builtin = false;

		if (mn::map_lookup(d->tags.table, KEYWORD_BUILTIN))
			is_builtin = true;

		if (is_builtin)
			return;

		auto return_type = t->func.return_type;
		mn::print_to(self.out, "{} {}(", _hlsl_write_field(self, return_type, ""), _hlsl_name(self, name));

		if (d->func_decl.body != nullptr)
			_hlsl_enter_scope(self, unit_scope_find(self.unit->parent_unit, d));
		mn_defer(if (d->func_decl.body) _hlsl_leave_scope(self));

		size_t i = 0;
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = t->func.args.types[i];
			for (auto name: arg.names)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
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
		_hlsl_func_gen_internal(self, sym->func_sym.decl, sym->type, _hlsl_symbol_name(sym));
	}

	inline static void
	_hlsl_var_gen(HLSL& self, Symbol* sym, bool in_stmt)
	{
		if (sym->var_sym.value && in_stmt == false)
			_hlsl_rewrite_complits_in_expr(self, sym->var_sym.value, false);

		if (sym->var_sym.is_uniform)
		{
			auto uniform_name = _hlsl_name(self, _hlsl_symbol_name(sym));
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
							mn::print_to(self.out, "{};", _hlsl_write_field(self, field.type, name.ptr));
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
		else
		{
			mn::print_to(self.out, "{}", _hlsl_write_field(self, sym->type, _hlsl_symbol_name(sym)));
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

		mn::print_to(self.out, "const {}", _hlsl_write_field(self, sym->type, _hlsl_symbol_name(sym)));
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
	_hlsl_struct_gen(HLSL& self, Symbol* sym)
	{
		mn::print_to(self.out, "struct {} {{", _hlsl_name(self, _hlsl_symbol_name(sym)));
		++self.indent;

		auto d = sym->struct_sym.decl;
		auto t = sym->type;

		auto io_flags_it = mn::map_lookup(self.io_structs, sym);

		size_t i = 0;
		for (auto field: d->struct_decl.fields)
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
			}
			i += field.names.count;
		}

		--self.indent;
		_hlsl_newline(self);
		mn::print_to(self.out, "}};");
	}

	inline static void
	_hlsl_enum_gen(HLSL& self, Symbol* sym)
	{
		mn::print_to(self.out, "enum {} {{", _hlsl_name(self, _hlsl_symbol_name(sym)));
		++self.indent;

		auto d = sym->enum_sym.decl;
		auto t = sym->type;

		size_t i = 0;
		for (auto field: d->enum_decl.fields)
		{
			if (i > 0)
				mn::print_to(self.out, ", ");
			auto field_type = t->enum_type.fields[i];
			assert(field_type.value.type == type_int);
			_hlsl_newline(self);
			mn::print_to(self.out, "{}_{} = {}", _hlsl_symbol_name(sym), field.name.str, field_type.value.as_int);
			++i;
		}

		--self.indent;
		_hlsl_newline(self);
		mn::print_to(self.out, "}};");
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
				_hlsl_func_gen_internal(self, decl, type, _hlsl_symbol_name(sym, decl));
				was_last_symbol_generated = _hlsl_code_generated_after(self, pos);
			}
			break;
		}
		case Symbol::KIND_STRUCT:
			_hlsl_struct_gen(self, sym);
			break;
		case Symbol::KIND_PACKAGE:
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
			break;
		}
		case Symbol::KIND_ENUM:
			_hlsl_enum_gen(self, sym);
			break;
		default:
			assert(false && "unreachable");
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
				auto arg_type = entry_type->func.args.types[type_index++];
				switch(arg_type->kind)
				{
				case Type::KIND_STRUCT:
					mn::map_insert(self.io_structs, arg_type->struct_type.symbol, ENTRY_IO_FLAG_NONE);
					break;
				default:
					assert(false && "unreachable");
					break;
				}
			}
		}

		size_t out_location = 0;
		if (entry_type->func.return_type != type_void)
		{
			auto ret_type = entry_type->func.return_type;

			switch(ret_type->kind)
			{
			case Type::KIND_STRUCT:
				mn::map_insert(self.io_structs, ret_type->struct_type.symbol, ENTRY_IO_FLAG_NONE);
				break;
			default:
				assert(false && "unreachable");
				break;
			}
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
				auto arg_type = entry_type->func.args.types[type_index++];
				switch(arg_type->kind)
				{
				case Type::KIND_STRUCT:
					mn::map_insert(self.io_structs, arg_type->struct_type.symbol, ENTRY_IO_FLAG_NONE);
					break;
				default:
					assert(false && "unreachable");
					break;
				}
			}
		}

		size_t out_location = 0;
		if (entry_type->func.return_type != type_void)
		{
			auto ret_type = entry_type->func.return_type;

			switch(ret_type->kind)
			{
			case Type::KIND_STRUCT:
				mn::map_insert(self.io_structs, ret_type->struct_type.symbol, ENTRY_IO_FLAG_PIXEL_OUT);
				break;
			default:
				assert(false && "unreachable");
				break;
			}
		}

		if (out_location > 0)
			_hlsl_newline(self);
	}

	inline static void
	_hlsl_generate_main_func(HLSL& self, Symbol* entry)
	{
		assert(entry->kind == Symbol::KIND_FUNC);

		auto d = entry->func_sym.decl;
		auto t = entry->type;
		auto return_type = t->func.return_type;
		mn::print_to(self.out, "{} main(", _hlsl_write_field(self, return_type, ""));

		if (d->func_decl.body != nullptr)
			_hlsl_enter_scope(self, unit_scope_find(self.unit->parent_unit, d));
		mn_defer(if (d->func_decl.body) _hlsl_leave_scope(self));

		size_t i = 0;
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = t->func.args.types[i];
			for (auto name: arg.names)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				mn::print_to(self.out, "{}", _hlsl_write_field(self, arg_type, name.str));
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

		mn::print_to(self.out, "{}(", _hlsl_name(self, _hlsl_symbol_name(entry)));
		i = 0;
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = t->func.args.types[i];
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

		// push global scope as first entry in scope stack
		auto global_scope = self.unit->global_scope;
		assert(global_scope != nullptr);
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
		mn::buf_free(self.scope_stack);
		mn::map_free(self.reserved_to_alternative);
		mn::map_free(self.symbol_to_names);
		mn::set_free(self.io_structs);
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

	void
	hlsl_stmt_gen(HLSL& self, Stmt* s)
	{
		// handle compound literal expressions
		bool is_const = s->kind == Stmt::KIND_DECL && s->decl_stmt->kind == Decl::KIND_CONST;
		_hlsl_rewrite_complits_in_stmt(self, s, is_const);

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
			assert(false && "unreachable");
			break;
		}
	}

	void
	hlsl_gen(HLSL& self)
	{
		auto compilation_unit = self.unit->parent_unit;

		switch (compilation_unit->mode)
		{
		case COMPILATION_MODE_LIBRARY:
			// do nothing
			break;
		case COMPILATION_MODE_VERTEX:
			_hlsl_generate_vertex_shader_io(self, compilation_unit->entry_symbol);
			break;
		case COMPILATION_MODE_PIXEL:
			_hlsl_generate_pixel_shader_io(self, compilation_unit->entry_symbol);
			break;
		default:
			assert(false && "unreachable");
			break;
		}

		bool last_symbol_was_generated = false;
		for (size_t i = 0; i < self.unit->reachable_symbols.count; ++i)
		{
			if (last_symbol_was_generated)
				_hlsl_newline(self);

			auto pos = _hlsl_buffer_position(self);
			_hlsl_symbol_gen(self, self.unit->reachable_symbols[i], false);
			last_symbol_was_generated = _hlsl_code_generated_after(self, pos);
		}

		// generate real entry function
		if (compilation_unit->mode != COMPILATION_MODE_LIBRARY)
		{
			_hlsl_newline(self);
			_hlsl_newline(self);
			_hlsl_generate_main_func(self, compilation_unit->entry_symbol);
		}
	}
}