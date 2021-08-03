#include "sabre/GLSL.h"
#include "mn/Fmt.h"
#include "mn/Rune.h"
#include "mn/Str.h"
#include "sabre/Unit.h"
#include "sabre/Type_Interner.h"
#include "sabre/AST.h"

#include <mn/Defer.h>
#include <mn/Log.h>

namespace sabre
{
	inline static const char* GLSL_KEYWORDS[] = {
		"attribute",
		"const",
		"uniform",
		"varying",
		"buffer",
		"shared",
		"coherent",
		"volatile",
		"restrict",
		"readonly",
		"writeonly",
		"atomic_uint",
		"layout",
		"centroid",
		"flat",
		"smooth",
		"noperspective",
		"patch",
		"sample",
		"break",
		"continue",
		"do",
		"for",
		"while",
		"switch",
		"case",
		"default",
		"if",
		"else",
		"subroutine",
		"in",
		"out",
		"inout",
		"float",
		"double",
		"int",
		"void",
		"bool",
		"true",
		"false",
		"invariant",
		"precise",
		"discard",
		"return",
		"mat2",
		"mat3",
		"mat4",
		"dmat2",
		"dmat3",
		"dmat4",
		"mat2x2",
		"mat2x3",
		"mat2x4",
		"dmat2x2",
		"dmat2x3",
		"dmat2x4",
		"mat3x2",
		"mat3x3",
		"mat3x4",
		"dmat3x2",
		"dmat3x3",
		"dmat3x4",
		"mat4x2",
		"mat4x3",
		"mat4x4",
		"dmat4x2",
		"dmat4x3",
		"dmat4x4",
		"vec2",
		"vec3",
		"vec4",
		"ivec2",
		"ivec3",
		"ivec4",
		"bvec2",
		"bvec3",
		"bvec4",
		"dvec2",
		"dvec3",
		"dvec4",
		"uint",
		"uvec2",
		"uvec3",
		"uvec4",
		"lowp",
		"mediump",
		"highp",
		"precision",
		"sampler1D",
		"sampler2D",
		"sampler3D",
		"samplerCube",
		"sampler1DShadow",
		"sampler2DShadow",
		"samplerCubeShadow",
		"sampler1DArray",
		"sampler2DArray",
		"sampler1DArrayShadow",
		"sampler2DArrayShadow",
		"isampler1D",
		"isampler2D",
		"isampler3D",
		"isamplerCube",
		"isampler1DArray",
		"isampler2DArray",
		"usampler1D",
		"usampler2D",
		"usampler3D",
		"usamplerCube",
		"usampler1DArray",
		"usampler2DArray",
		"sampler2DRect",
		"sampler2DRectShadow",
		"isampler2D",
		"Rect",
		"usampler2DRect",
		"samplerBuffer",
		"isamplerBuffer",
		"usamplerBuffer",
		"sampler2DMS",
		"isampler2DMS",
		"usampler2DMS",
		"sampler2DMSArray",
		"isampler2DMSArray",
		"usampler2DMSArray",
		"samplerCubeArray",
		"samplerCubeArrayShadow",
		"isamplerCubeArray",
		"usamplerCubeArray",
		"image1D",
		"iimage1D",
		"uimage1D",
		"image2D",
		"iimage2D",
		"uimage2D",
		"image3D",
		"iimage3D",
		"uimage3D",
		"image2DRect",
		"iimage2DRect",
		"uimage2DRect",
		"imageCube",
		"iimageCube",
		"uimageCube",
		"imageBuffer",
		"iimageBuffer",
		"uimageBuffer",
		"image1DArray",
		"iimage1DArray",
		"uimage1DArray",
		"image2DArray",
		"iimage2DArray",
		"uimage2DArray",
		"imageCubeArray",
		"iimageCubeArray",
		"uimageCubeArray",
		"image2DMS",
		"iimage2DMS",
		"uimage2DMS",
		"image2DMSArray",
		"iimage2DMSArray",
		"uimage2DMSArraystruct",
		"common",
		"partition",
		"active",
		"asm",
		"class",
		"union",
		"enum",
		"typedef",
		"template",
		"this",
		"resource",
		"goto",
		"inline",
		"noinline",
		"public",
		"static",
		"extern",
		"external",
		"interface",
		"long",
		"short",
		"half",
		"fixed",
		"unsigned",
		"superp",
		"input",
		"output",
		"hvec2",
		"hvec3",
		"hvec4",
		"fvec2",
		"fvec3",
		"fvec4",
		"sampler3DRect",
		"filter",
		"sizeof",
		"cast",
		"namespace",
		"using",
		"main",
	};

	inline static int64_t
	_glsl_buffer_position(GLSL& self)
	{
		return mn::stream_cursor_pos(self.out);
	}

	inline static bool
	_glsl_code_generated_after(GLSL& self, int64_t pos)
	{
		return mn::stream_cursor_pos(self.out) > pos;
	}

	inline static const char*
	_glsl_name(GLSL& self, const char* name)
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

	inline static void
	_glsl_newline(GLSL& self)
	{
		mn::print_to(self.out, "\n");
		for (size_t i = 0; i < self.indent; ++i)
			mn::print_to(self.out, "\t");
	}

	inline static void
	_glsl_enter_scope(GLSL& self, Scope* scope)
	{
		assert(scope != nullptr);
		mn::buf_push(self.scope_stack, scope);
	}

	inline static void
	_glsl_leave_scope(GLSL& self)
	{
		assert(self.scope_stack.count > 1);
		mn::buf_pop(self.scope_stack);
	}

	inline static Scope*
	_glsl_current_scope(GLSL& self)
	{
		return mn::buf_top(self.scope_stack);
	}

	inline static Symbol*
	_glsl_find_symbol(GLSL& self, const char* name)
	{
		auto current_scope = _glsl_current_scope(self);
		return scope_find(current_scope, name);
	}

	inline static const char*
	_glsl_symbol_name(Symbol* sym, Decl* decl = nullptr)
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
	_glsl_tmp_name(GLSL& self)
	{
		auto scope = _glsl_current_scope(self);
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
		return _glsl_name(self, interned_res);
	}

	inline static mn::Str
	_glsl_write_field(GLSL& self, mn::Str str, Type* type, const char* name)
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
					str = mn::strf(str, "bvec2");
					break;
				case 3:
					str = mn::strf(str, "bvec3");
					break;
				case 4:
					str = mn::strf(str, "bvec4");
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
					str = mn::strf(str, "ivec2");
					break;
				case 3:
					str = mn::strf(str, "ivec3");
					break;
				case 4:
					str = mn::strf(str, "ivec4");
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
					str = mn::strf(str, "uvec2");
					break;
				case 3:
					str = mn::strf(str, "uvec3");
					break;
				case 4:
					str = mn::strf(str, "uvec4");
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
					str = mn::strf(str, "vec2");
					break;
				case 3:
					str = mn::strf(str, "vec3");
					break;
				case 4:
					str = mn::strf(str, "vec4");
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
					str = mn::strf(str, "dvec2");
					break;
				case 3:
					str = mn::strf(str, "dvec3");
					break;
				case 4:
					str = mn::strf(str, "dvec4");
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
				str = mn::strf(str, "mat2");
			}
			else if (type == type_mat3)
			{
				str = mn::strf(str, "mat3");
			}
			else if (type == type_mat4)
			{
				str = mn::strf(str, "mat4");
			}
			else
			{
				assert(false && "unreachable");
			}
			break;
		case Type::KIND_STRUCT:
			can_write_name = true;
			str = mn::strf(str, "{}", _glsl_name(self, _glsl_symbol_name(type->struct_type.symbol)));
			break;
		case Type::KIND_TEXTURE:
			can_write_name = true;
			if (type == type_texture1d)
			{
				str = mn::strf(str, "sampler1D");
			}
			else if (type == type_texture2d)
			{
				str = mn::strf(str, "sampler2D");
			}
			else if (type == type_texture3d)
			{
				str = mn::strf(str, "sampler3D");
			}
			else if (type == type_texture_cube)
			{
				str = mn::strf(str, "samplerCube");
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
			str = _glsl_write_field(self, str, type->array.base, array_name.ptr);
			break;
		}
		case Type::KIND_ENUM:
			can_write_name = true;
			str = mn::strf(str, "int");
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
				str = mn::strf(str, " {}", _glsl_name(self, name));
		}
		return str;
	}

	inline static mn::Str
	_glsl_write_field(GLSL& self, Type* type, const char* name)
	{
		return _glsl_write_field(self, mn::str_tmp(), type, name);
	}

	inline static void
	_glsl_zero_value(GLSL& self, Type* t)
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
			mn::print_to(self.out, "{}(", _glsl_write_field(self, t, nullptr));
			for (size_t i = 0; i < t->vec.width; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				_glsl_zero_value(self, t->vec.base);
			}
			mn::print_to(self.out, ")");
			break;
		case Type::KIND_MAT:
			mn::print_to(self.out, "{}(", _glsl_write_field(self, t, nullptr));
			for (size_t i = 0; i < t->mat.width * t->mat.width; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				_glsl_zero_value(self, t->mat.base);
			}
			mn::print_to(self.out, ")");
			break;
		case Type::KIND_ARRAY:
			mn::print_to(self.out, "{}(", _glsl_write_field(self, t, nullptr));
			for (size_t i = 0; i < t->array.count; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				_glsl_zero_value(self, t->array.base);
			}
			mn::print_to(self.out, ")");
			break;
		case Type::KIND_STRUCT:
			mn::print_to(self.out, "{}(", _glsl_write_field(self, t, nullptr));
			for (size_t i = 0; i < t->struct_type.fields.count; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				_glsl_zero_value(self, t->struct_type.fields[i].type);
			}
			mn::print_to(self.out, ")");
			break;
		case Type::KIND_ENUM:
			mn::print_to(self.out, "{}(0)", _glsl_write_field(self, t, nullptr));
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
	_glsl_gen_atom_expr(GLSL& self, Expr* e)
	{
		if (e->atom.tkn.kind == Tkn::KIND_ID)
		{
			if (e->atom.sym)
			{
				if (::strcmp(e->atom.tkn.str, "dot") == 0)
				{
					int x = 234;
				}

				auto package_name = mn::str_lit(_glsl_symbol_name(e->atom.sym, e->atom.decl));
				if (package_name.count > 0)
				{
					mn::print_to(self.out, "{}", _glsl_name(self, package_name.ptr));
					return;
				}
			}
		}

		if (e->atom.tkn.kind == Tkn::KIND_ID)
		{
			mn::print_to(self.out, "{}", _glsl_name(self, e->atom.tkn.str));
		}
		else
		{
			mn::print_to(self.out, "{}", e->atom.tkn.str);
		}
	}

	inline static void
	_glsl_gen_binary_expr(GLSL& self, Expr* e)
	{
		glsl_expr_gen(self, e->binary.left);
		mn::print_to(self.out, " {} ", e->binary.op.str);
		glsl_expr_gen(self, e->binary.right);
	}

	inline static void
	_glsl_gen_unary_expr(GLSL& self, Expr* e)
	{
		mn::print_to(self.out, "{}", e->unary.op.str);
		glsl_expr_gen(self, e->unary.base);
	}

	inline static void
	_glsl_gen_dot_expr(GLSL& self, Expr* e)
	{
		auto lhs = e->dot.lhs;
		bool is_lhs_package = false;
		bool is_lhs_enum = type_is_enum(e->type);

		if (lhs && lhs->kind == Expr::KIND_ATOM)
			is_lhs_package = lhs->atom.sym->kind == Symbol::KIND_PACKAGE;

		if (is_lhs_package)
		{
			glsl_expr_gen(self, e->dot.rhs);
		}
		else if (is_lhs_enum)
		{
			mn::print_to(self.out, "{}_", _glsl_symbol_name(e->type->enum_type.symbol));
			glsl_expr_gen(self, e->dot.rhs);
		}
		else
		{
			glsl_expr_gen(self, e->dot.lhs);
			const char* dot = ".";
			if (e->dot.lhs->kind == Expr::KIND_ATOM)
				if (auto decl = e->dot.lhs->atom.decl)
					if (auto it = mn::map_lookup(decl->tags.table, KEYWORD_UNIFORM))
						dot = "_";
			mn::print_to(self.out, "{}", dot);
			glsl_expr_gen(self, e->dot.rhs);
		}
	}

	inline static void
	_glsl_gen_indexed_expr(GLSL& self, Expr* e)
	{
		glsl_expr_gen(self, e->indexed.base);
		mn::print_to(self.out, "[");
		glsl_expr_gen(self, e->indexed.index);
		mn::print_to(self.out, "]");
	}

	inline static void
	_glsl_gen_call_expr(GLSL& self, Expr* e)
	{
		glsl_expr_gen(self, e->call.base);
		mn::print_to(self.out, "(");
		for (size_t i = 0; i < e->call.args.count; ++i)
		{
			if (i > 0)
				mn::print_to(self.out, ", ");
			glsl_expr_gen(self, e->call.args[i]);
		}
		mn::print_to(self.out, ")");
	}

	inline static void
	_glsl_gen_cast_expr(GLSL& self, Expr* e)
	{
		mn::print_to(self.out, "{}(", _glsl_write_field(self, e->type, ""));
		glsl_expr_gen(self, e->cast.base);
		mn::print_to(self.out, ")");
	}

	inline static void
	_glsl_gen_complit_expr(GLSL& self, Expr* e)
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

	inline static void
	_glsl_gen_break_stmt(GLSL& self, Stmt* s)
	{
		mn::print_to(self.out, "break");
	}

	inline static void
	_glsl_gen_continue_stmt(GLSL& self, Stmt* s)
	{
		mn::print_to(self.out, "continue");
	}

	inline static void
	_glsl_gen_return_stmt(GLSL& self, Stmt* s)
	{
		if (s->return_stmt)
		{
			mn::print_to(self.out, "return ");
			glsl_expr_gen(self, s->return_stmt);
		}
		else
		{
			mn::print_to(self.out, "return");
		}
	}

	inline static void
	_glsl_gen_block_stmt(GLSL& self, Stmt* s);

	inline static void
	_glsl_gen_if_stmt(GLSL& self, Stmt* s)
	{
		for (size_t i = 0; i < s->if_stmt.body.count; ++i)
		{
			if (i > 0)
				mn::print_to(self.out, " else ");

			mn::print_to(self.out, "if (");
			glsl_expr_gen(self, s->if_stmt.cond[i]);
			mn::print_to(self.out, ") ");
			_glsl_gen_block_stmt(self, s->if_stmt.body[i]);
		}

		if (s->if_stmt.else_body != nullptr)
		{
			mn::print_to(self.out, " else ");
			_glsl_gen_block_stmt(self, s->if_stmt.else_body);
		}
	}

	inline static bool
	_glsl_add_semicolon_after(GLSL& self, Stmt* s)
	{
		if (s->kind == Stmt::KIND_BREAK ||
			s->kind == Stmt::KIND_CONTINUE ||
			s->kind == Stmt::KIND_RETURN ||
			s->kind == Stmt::KIND_ASSIGN ||
			s->kind == Stmt::KIND_EXPR)
		{
			return true;
		}
		return false;
	}

	inline static void
	_glsl_gen_for_stmt(GLSL& self, Stmt* s)
	{
		_glsl_enter_scope(self, unit_scope_find(self.unit->parent_unit, s));
		mn_defer(_glsl_leave_scope(self));

		mn::print_to(self.out, "{{ // for scope");
		++self.indent;

		_glsl_newline(self);
		mn::print_to(self.out, "// for init statement");
		if (s->for_stmt.init != nullptr)
		{
			_glsl_newline(self);
			glsl_stmt_gen(self, s->for_stmt.init);
			if (_glsl_add_semicolon_after(self, s->for_stmt.init))
				mn::print_to(self.out, ";");
		}

		_glsl_newline(self);
		mn::print_to(self.out, "while (");
		if (s->for_stmt.cond != nullptr)
			glsl_expr_gen(self, s->for_stmt.cond);
		else
			mn::print_to(self.out, "true");
		mn::print_to(self.out, ") {{");
		++self.indent;

		_glsl_newline(self);
		mn::print_to(self.out, "// for body");
		for (auto stmt: s->for_stmt.body->block_stmt)
		{
			_glsl_newline(self);
			glsl_stmt_gen(self, stmt);
			if (_glsl_add_semicolon_after(self, stmt))
			{
				mn::print_to(self.out, ";");
			}
		}

		if (s->for_stmt.post != nullptr)
		{
			_glsl_newline(self);
			mn::print_to(self.out, "// for post statement");
			_glsl_newline(self);
			glsl_stmt_gen(self, s->for_stmt.post);
			mn::print_to(self.out, ";");
		}

		--self.indent;
		_glsl_newline(self);
		mn::print_to(self.out, "}}");

		--self.indent;
		_glsl_newline(self);
		mn::print_to(self.out, "}} // for scope");
	}

	inline static void
	_glsl_assign(GLSL& self, Expr* lhs, const char* op, Expr* rhs)
	{
		glsl_expr_gen(self, lhs);
		mn::print_to(self.out, " {} ", op);
		glsl_expr_gen(self, rhs);
	}

	inline static void
	_glsl_assign(GLSL& self, Symbol* lhs, const char* op, Expr* rhs)
	{
		auto e = expr_atom_new(self.unit->symbols_arena, lhs->var_sym.name);
		e->type = lhs->type;
		e->mode = ADDRESS_MODE_VARIABLE;
		_glsl_assign(self, e, op, rhs);
	}

	inline static void
	_glsl_gen_assign_stmt(GLSL& self, Stmt* s)
	{
		for (size_t i = 0; i < s->assign_stmt.lhs.count; ++i)
		{
			if (i > 0)
			{
				mn::print_to(self.out, ";");
				_glsl_newline(self);
			}

			auto lhs = s->assign_stmt.lhs[i];
			auto rhs = s->assign_stmt.rhs[i];

			_glsl_assign(self, lhs, s->assign_stmt.op.str, rhs);
		}
	}

	inline static void
	_glsl_gen_block_stmt(GLSL& self, Stmt* s)
	{
		mn::print_to(self.out, "{{");
		++self.indent;

		for (auto stmt: s->block_stmt)
		{
			_glsl_newline(self);
			glsl_stmt_gen(self, stmt);
			if (_glsl_add_semicolon_after(self, stmt))
			{
				mn::print_to(self.out, ";");
			}
		}

		--self.indent;
		_glsl_newline(self);
		mn::print_to(self.out, "}}");
	}

	inline static void
	_glsl_func_gen_internal(GLSL& self, Decl* d, Type* t, const char* name)
	{
		bool is_builtin = false;

		if (mn::map_lookup(d->tags.table, KEYWORD_BUILTIN))
			is_builtin = true;

		if (is_builtin)
			return;

		auto return_type = t->func.return_type;
		mn::print_to(self.out, "{} {}(", _glsl_write_field(self, return_type, ""), _glsl_name(self, name));

		if (d->func_decl.body != nullptr)
			_glsl_enter_scope(self, unit_scope_find(self.unit->parent_unit, d));
		mn_defer(if (d->func_decl.body) _glsl_leave_scope(self));

		size_t i = 0;
		for (auto arg: d->func_decl.args)
		{
			auto arg_type = t->func.args.types[i];
			for (auto name: arg.names)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				mn::print_to(self.out, "{}", _glsl_write_field(self, arg_type, name.str));
				++i;
			}
		}

		mn::print_to(self.out, ")");

		if (d->func_decl.body != nullptr)
		{
			mn::print_to(self.out, " ");
			_glsl_gen_block_stmt(self, d->func_decl.body);
		}
	}

	inline static void
	_glsl_func_gen(GLSL& self, Symbol* sym)
	{
		_glsl_func_gen_internal(self, sym->func_sym.decl, sym->type, _glsl_symbol_name(sym));
	}

	inline static void
	_glsl_rewrite_complits_in_expr(GLSL& self, Expr* e, bool is_const);

	inline static void
	_glsl_var_gen(GLSL& self, Symbol* sym, bool in_stmt)
	{
		if (sym->var_sym.value && in_stmt == false)
			_glsl_rewrite_complits_in_expr(self, sym->var_sym.value, false);

		auto decl = symbol_decl(sym);

		if (auto it = mn::map_lookup(decl->tags.table, KEYWORD_UNIFORM))
		{
			int binding = -1;
			if (auto arg_it = mn::map_lookup(it->value.args, KEYWORD_BINDING))
			{
				auto value_tkn = arg_it->value.value;
				if (value_tkn.kind == Tkn::KIND_LITERAL_INTEGER)
				{
					binding = ::atoi(value_tkn.str);
					if (binding > self.uniform_binding_generator)
						self.uniform_binding_generator = binding + 1;
				}
			}
			// if no user generated uniform binding is found we generate one for it
			if (binding < 0)
				binding = self.uniform_binding_generator++;

			auto uniform_name = _glsl_name(self, _glsl_symbol_name(sym));
			auto uniform_block_name = uniform_name;
			if (sym->type->kind != Type::KIND_STRUCT)
			{
				uniform_block_name = _glsl_name(self, mn::str_tmpf("uniform{}", _glsl_tmp_name(self)).ptr);
			}

			if (sym->type->kind == Type::KIND_TEXTURE)
			{
				mn::print_to(self.out, "layout(binding = {}) uniform {}", binding, _glsl_write_field(self, sym->type, uniform_name));
			}
			else
			{
				mn::print_to(self.out, "layout(binding = {}, std140) uniform {} {{", binding, uniform_block_name);
				++self.indent;
				{
					auto type = sym->type;
					if (type->kind == Type::KIND_STRUCT)
					{
						for (auto field: type->struct_type.fields)
						{
							_glsl_newline(self);
							auto name = mn::str_tmpf("{}_{}", uniform_name, field.name.str);
							mn::print_to(self.out, "{};", _glsl_write_field(self, field.type, name.ptr));
						}
					}
					else
					{
						_glsl_newline(self);
						mn::print_to(self.out, "{};", _glsl_write_field(self, type, uniform_name));
					}
				}
				--self.indent;
				_glsl_newline(self);
				mn::print_to(self.out, "}}");
			}
		}
		else
		{
			mn::print_to(self.out, "{}", _glsl_write_field(self, sym->type, _glsl_symbol_name(sym)));
			if (sym->var_sym.value != nullptr)
			{
				mn::print_to(self.out, " = ");
				glsl_expr_gen(self, sym->var_sym.value);
			}
			else
			{
				// TODO(Moustapha): handle zero init data
			}
		}
		mn::print_to(self.out, ";");
	}

	inline static void
	_glsl_const_gen(GLSL& self, Symbol* sym, bool in_stmt)
	{
		if (sym->const_sym.value && in_stmt == false)
			_glsl_rewrite_complits_in_expr(self, sym->const_sym.value, true);

		mn::print_to(self.out, "const {}", _glsl_write_field(self, sym->type, _glsl_symbol_name(sym)));
		if (sym->const_sym.value != nullptr)
		{
			mn::print_to(self.out, " = ");
			glsl_expr_gen(self, sym->var_sym.value);
		}
		else
		{
			// TODO(Moustapha): handle zero init data
		}
		mn::print_to(self.out, ";");
	}

	inline static void
	_glsl_struct_gen(GLSL& self, Symbol* sym)
	{
		mn::print_to(self.out, "struct {} {{", _glsl_name(self, _glsl_symbol_name(sym)));
		++self.indent;

		auto d = sym->struct_sym.decl;
		auto t = sym->type;

		size_t i = 0;
		for (auto field: d->struct_decl.fields)
		{
			auto field_type = t->struct_type.fields[i];
			for (auto name: field.names)
			{
				_glsl_newline(self);
				mn::print_to(self.out, "{};", _glsl_write_field(self, field_type.type, name.str));
			}
			i += field.names.count;
		}

		--self.indent;
		_glsl_newline(self);
		mn::print_to(self.out, "}};");
	}

	inline static void
	_glsl_enum_gen(GLSL& self, Symbol* sym)
	{
		auto d = sym->enum_sym.decl;
		auto t = sym->type;

		size_t i = 0;
		for (auto field: d->enum_decl.fields)
		{
			auto field_type = t->enum_type.fields[i];
			assert(field_type.value.type == type_int);
			_glsl_newline(self);
			mn::print_to(self.out, "#define {}_{} {}", _glsl_symbol_name(sym), field.name.str, field_type.value.as_int);
			++i;
		}
	}

	inline static void
	_glsl_symbol_gen(GLSL& self, Symbol* sym, bool in_stmt)
	{
		switch (sym->kind)
		{
		case Symbol::KIND_FUNC:
			_glsl_func_gen(self, sym);
			break;
		case Symbol::KIND_VAR:
			_glsl_var_gen(self, sym, in_stmt);
			break;
		case Symbol::KIND_CONST:
			_glsl_const_gen(self, sym, in_stmt);
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
					_glsl_newline(self);

				auto pos = _glsl_buffer_position(self);
				_glsl_func_gen_internal(self, decl, type, _glsl_symbol_name(sym, decl));
				was_last_symbol_generated = _glsl_code_generated_after(self, pos);
			}
			break;
		}
		case Symbol::KIND_STRUCT:
			_glsl_struct_gen(self, sym);
			break;
		case Symbol::KIND_PACKAGE:
		{
			auto package = sym->package_sym.package;
			if (package->stage == COMPILATION_STAGE_CODEGEN)
			{
				for (size_t i = 0; i < package->reachable_symbols.count; ++i)
				{
					if (i > 0)
						_glsl_newline(self);
					_glsl_symbol_gen(self, package->reachable_symbols[i], in_stmt);
				}
				if (package->reachable_symbols.count > 0)
					_glsl_newline(self);
				package->stage = COMPILATION_STAGE_SUCCESS;
			}
			break;
		}
		case Symbol::KIND_ENUM:
			_glsl_enum_gen(self, sym);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_glsl_gen_decl_stmt(GLSL& self, Stmt* s)
	{
		auto scope = _glsl_current_scope(self);
		auto d = s->decl_stmt;
		switch (d->kind)
		{
		case Decl::KIND_VAR:
		{
			for (size_t i = 0; i < d->var_decl.names.count; ++i)
			{
				if (i > 0)
					_glsl_newline(self);
				auto name = d->var_decl.names[i];
				_glsl_symbol_gen(self, scope_find(scope, name.str), true);
			}
			break;
		}
		case Decl::KIND_CONST:
		{
			for (size_t i = 0; i < d->const_decl.names.count; ++i)
			{
				if (i > 0)
					_glsl_newline(self);
				auto name = d->const_decl.names[i];
				_glsl_symbol_gen(self, scope_find(scope, name.str), true);
			}
			break;
		}
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_glsl_rewrite_complits_in_binary_expr(GLSL& self, Expr* e, bool is_const)
	{
		_glsl_rewrite_complits_in_expr(self, e->binary.left, is_const);
		_glsl_rewrite_complits_in_expr(self, e->binary.right, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_unary_expr(GLSL& self, Expr* e, bool is_const)
	{
		_glsl_rewrite_complits_in_expr(self, e->unary.base, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_dot_expr(GLSL& self, Expr* e, bool is_const)
	{
		if (e->dot.lhs)
			_glsl_rewrite_complits_in_expr(self, e->dot.lhs, is_const);
		_glsl_rewrite_complits_in_expr(self, e->dot.rhs, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_indexed_expr(GLSL& self, Expr* e, bool is_const)
	{
		_glsl_rewrite_complits_in_expr(self, e->indexed.base, is_const);
		_glsl_rewrite_complits_in_expr(self, e->indexed.index, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_call_expr(GLSL& self, Expr* e, bool is_const)
	{
		_glsl_rewrite_complits_in_expr(self, e->call.base, is_const);
		for (auto arg: e->call.args)
			_glsl_rewrite_complits_in_expr(self, arg, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_cast_expr(GLSL& self, Expr* e, bool is_const)
	{
		_glsl_rewrite_complits_in_expr(self, e->cast.base, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_complit_expr(GLSL& self, Expr* e, bool is_const)
	{
		for (size_t i = 0; i < e->complit.fields.count; ++i)
		{
			if (e->complit.fields[i].value)
				_glsl_rewrite_complits_in_expr(self, e->complit.fields[i].value, is_const);
		}

		if (is_const)
			mn::print_to(self.out, "const ");

		// handle arrays differently
		if (type_is_array(e->type))
		{
			auto tmp_name = _glsl_tmp_name(self);
			mn::map_insert(self.symbol_to_names, (void*)e, tmp_name);
			mn::print_to(self.out, "{} = {}(", _glsl_write_field(self, e->type, tmp_name), _glsl_write_field(self, e->type, nullptr));
			for (size_t i = 0; i < e->type->array.count; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");
				if (i < e->complit.fields.count)
					glsl_expr_gen(self, e->complit.fields[i].value);
				else
					_glsl_zero_value(self, e->type->array.base);
			}
			mn::print_to(self.out, ");");
			_glsl_newline(self);
		}
		else if (type_is_vec(e->type))
		{
			auto tmp_name = _glsl_tmp_name(self);
			mn::map_insert(self.symbol_to_names, (void*)e, tmp_name);
			mn::print_to(self.out, "{} = {}(", _glsl_write_field(self, e->type, tmp_name), _glsl_write_field(self, e->type, nullptr));
			for (size_t i = 0; i < e->type->vec.width; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");

				if (auto field_it = mn::map_lookup(e->complit.referenced_fields, i))
				{
					auto field = e->complit.fields[field_it->value];
					glsl_expr_gen(self, field.value);
					// advance the field index by sub vector size to handle vector upcast cases
					if (type_is_vec(field.value->type))
						i += field.value->type->vec.width - 1;
				}
				else
				{
					_glsl_zero_value(self, e->type->vec.base);
				}
			}
			mn::print_to(self.out, ");");
			_glsl_newline(self);
		}
		else if (type_is_struct(e->type))
		{
			auto tmp_name = _glsl_tmp_name(self);
			mn::map_insert(self.symbol_to_names, (void*)e, tmp_name);
			mn::print_to(self.out, "{} = {}(", _glsl_write_field(self, e->type, tmp_name), _glsl_write_field(self, e->type, nullptr));
			for (size_t i = 0; i < e->type->struct_type.fields.count; ++i)
			{
				if (i > 0)
					mn::print_to(self.out, ", ");

				if (auto field_it = mn::map_lookup(e->complit.referenced_fields, i))
				{
					auto field = e->complit.fields[field_it->value];
					glsl_expr_gen(self, field.value);
				}
				else
				{
					_glsl_zero_value(self, e->type->struct_type.fields[i].type);
				}
			}
			mn::print_to(self.out, ");");
			_glsl_newline(self);
		}
		else
		{
			assert(false && "unreachable");
		}
	}

	inline static void
	_glsl_rewrite_complits_in_expr(GLSL& self, Expr* e, bool is_const)
	{
		switch (e->kind)
		{
		case Expr::KIND_ATOM:
			// do nothing, no complit here
			break;
		case Expr::KIND_BINARY:
			_glsl_rewrite_complits_in_binary_expr(self, e, is_const);
			break;
		case Expr::KIND_UNARY:
			_glsl_rewrite_complits_in_unary_expr(self, e, is_const);
			break;
		case Expr::KIND_DOT:
			_glsl_rewrite_complits_in_dot_expr(self, e, is_const);
			break;
		case Expr::KIND_INDEXED:
			_glsl_rewrite_complits_in_indexed_expr(self, e, is_const);
			break;
		case Expr::KIND_CALL:
			_glsl_rewrite_complits_in_call_expr(self, e, is_const);
			break;
		case Expr::KIND_CAST:
			_glsl_rewrite_complits_in_cast_expr(self, e, is_const);
			break;
		case Expr::KIND_COMPLIT:
			_glsl_rewrite_complits_in_complit_expr(self, e, is_const);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_glsl_rewrite_complits_in_stmt(GLSL& self, Stmt* s, bool is_const);

	inline static void
	_glsl_rewrite_complits_in_return_stmt(GLSL& self, Stmt* s, bool is_const)
	{
		_glsl_rewrite_complits_in_expr(self, s->return_stmt, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_if_stmt(GLSL& self, Stmt* s, bool is_const)
	{
		for (auto cond: s->if_stmt.cond)
			_glsl_rewrite_complits_in_expr(self, cond, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_for_stmt(GLSL& self, Stmt* s, bool is_const)
	{
		if (s->for_stmt.init)
			_glsl_rewrite_complits_in_stmt(self, s->for_stmt.init, is_const);
		if (s->for_stmt.cond)
			_glsl_rewrite_complits_in_expr(self, s->for_stmt.cond, is_const);
		if (s->for_stmt.post)
			_glsl_rewrite_complits_in_stmt(self, s->for_stmt.post, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_assign_stmt(GLSL& self, Stmt* s, bool is_const)
	{
		for (auto e: s->assign_stmt.lhs)
			_glsl_rewrite_complits_in_expr(self, e, is_const);

		for (auto e: s->assign_stmt.rhs)
			_glsl_rewrite_complits_in_expr(self, e, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_block_stmt(GLSL& self, Stmt* s, bool is_const)
	{
		for (auto stmt: s->block_stmt)
			_glsl_rewrite_complits_in_stmt(self, stmt, is_const);
	}

	inline static void
	_glsl_rewrite_complits_in_decl_stmt(GLSL& self, Stmt* s, bool is_const)
	{
		auto scope = _glsl_current_scope(self);
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
						_glsl_rewrite_complits_in_expr(self, sym->var_sym.value, is_const);
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
						_glsl_rewrite_complits_in_expr(self, sym->const_sym.value, is_const);
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
	_glsl_rewrite_complits_in_stmt(GLSL& self, Stmt* s, bool is_const)
	{
		switch (s->kind)
		{
		case Stmt::KIND_BREAK:
		case Stmt::KIND_CONTINUE:
			// do nothing, no complits here
			break;
		case Stmt::KIND_RETURN:
			_glsl_rewrite_complits_in_return_stmt(self, s, is_const);
			break;
		case Stmt::KIND_IF:
			_glsl_rewrite_complits_in_if_stmt(self, s, is_const);
			break;
		case Stmt::KIND_FOR:
			_glsl_rewrite_complits_in_for_stmt(self, s, is_const);
			break;
		case Stmt::KIND_ASSIGN:
			_glsl_rewrite_complits_in_assign_stmt(self, s, is_const);
			break;
		case Stmt::KIND_EXPR:
			_glsl_rewrite_complits_in_expr(self, s->expr_stmt, is_const);
			break;
		case Stmt::KIND_BLOCK:
			_glsl_rewrite_complits_in_block_stmt(self, s, is_const);
			break;
		case Stmt::KIND_DECL:
			_glsl_rewrite_complits_in_decl_stmt(self, s, is_const);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_glsl_generate_vertex_shader_io(GLSL& self, Symbol* entry)
	{
		auto decl = entry->func_sym.decl;
		auto entry_type = entry->type;
		size_t type_index = 0;
		size_t in_location = 0;
		// generate input
		for (size_t i = 0; i < decl->func_decl.args.count; ++i)
		{
			const auto& arg = decl->func_decl.args[i];

			for (const auto& name: arg.names)
			{
				auto input_name = _glsl_name(self, name.str);
				auto arg_type = entry_type->func.args.types[type_index++];
				switch(arg_type->kind)
				{
				case Type::KIND_STRUCT:
					for (auto field: arg_type->struct_type.fields)
					{
						auto field_name = mn::strf("{}_{}", input_name, field.name.str);
						mn::buf_push(self.input_names, field_name);
						mn::print_to(self.out, "layout(location = {}) in {};", in_location++, _glsl_write_field(self, field.type, field_name.ptr));
						_glsl_newline(self);
					}
					break;
				default:
					assert(false && "unreachable");
					break;
				}
			}
		}

		if (in_location > 0)
			_glsl_newline(self);

		size_t out_location = 0;
		if (entry_type->func.return_type != type_void)
		{
			auto ret_type = entry_type->func.return_type;
			auto output_name = _glsl_name(self, "_entry_point_output");

			switch(ret_type->kind)
			{
			case Type::KIND_STRUCT:
			{
				auto decl = symbol_decl(ret_type->struct_type.symbol);
				size_t field_index = 0;
				for (auto field: decl->struct_decl.fields)
				{
					if (mn::map_lookup(field.tags.table, KEYWORD_SV_POSITION) != nullptr)
					{
						mn::buf_push(self.output_names, mn::str_lit("gl_Position"));
						field_index++;
						continue;
					}

					auto type_field = ret_type->struct_type.fields[field_index++];
					auto field_name = mn::strf("{}_{}", output_name, type_field.name.str);
					mn::buf_push(self.output_names, field_name);
					mn::print_to(self.out, "layout(location = {}) out {};", out_location++, _glsl_write_field(self, type_field.type, field_name.ptr));
					_glsl_newline(self);
				}
				break;
			}
			default:
				assert(false && "unreachable");
				break;
			}
		}

		if (out_location > 0)
			_glsl_newline(self);
	}

	inline static void
	_glsl_generate_pixel_shader_io(GLSL& self, Symbol* entry)
	{
		auto decl = entry->func_sym.decl;
		auto entry_type = entry->type;
		size_t type_index = 0;
		size_t in_location = 0;
		// generate input
		for (size_t i = 0; i < decl->func_decl.args.count; ++i)
		{
			const auto& arg = decl->func_decl.args[i];

			for (const auto& name: arg.names)
			{
				auto input_name = _glsl_name(self, name.str);
				auto arg_type = entry_type->func.args.types[type_index++];
				switch(arg_type->kind)
				{
				case Type::KIND_STRUCT:
				{
					auto decl = symbol_decl(arg_type->struct_type.symbol);
					size_t field_index = 0;
					for (auto field: decl->struct_decl.fields)
					{
						if (mn::map_lookup(field.tags.table, KEYWORD_SV_POSITION) != nullptr)
						{
							mn::buf_push(self.input_names, mn::str_lit("gl_FragCoord"));
							field_index++;
							continue;
						}

						auto type_field = arg_type->struct_type.fields[field_index++];
						auto field_name = mn::strf("{}_{}", input_name, type_field.name.str);
						mn::buf_push(self.input_names, field_name);
						mn::print_to(self.out, "layout(location = {}) in {};", in_location++, _glsl_write_field(self, type_field.type, field_name.ptr));
						_glsl_newline(self);
					}
					break;
				}
				default:
					assert(false && "unreachable");
					break;
				}
			}
		}

		if (in_location > 0)
			_glsl_newline(self);

		size_t out_location = 0;
		if (entry_type->func.return_type != type_void)
		{
			auto ret_type = entry_type->func.return_type;
			auto output_name = _glsl_name(self, "_entry_point_output");

			switch(ret_type->kind)
			{
			case Type::KIND_STRUCT:
				for (auto field: ret_type->struct_type.fields)
				{
					auto field_name = mn::strf("{}_{}", output_name, field.name.str);
					mn::buf_push(self.output_names, field_name);
					mn::print_to(self.out, "layout(location = {}) out {};", out_location++, _glsl_write_field(self, field.type, field_name.ptr));
					_glsl_newline(self);
				}
				break;
			default:
				assert(false && "unreachable");
				break;
			}
		}

		if (out_location > 0)
			_glsl_newline(self);
	}

	inline static void
	_glsl_generate_main_func(GLSL& self, Symbol* entry)
	{
		mn::print_to(self.out, "void main() {{");
		++self.indent;
		_glsl_newline(self);
		{
			auto type = entry->type;
			auto decl = symbol_decl(entry);

			size_t type_index = 0;
			size_t input_index = 0;
			for (auto arg: decl->func_decl.args)
			{
				if (type_index > 0)
					_glsl_newline(self);

				auto arg_type = type->func.args.types[type_index];
				for (size_t i = 0; i < arg.names.count; ++i)
				{
					if (i > 0)
						_glsl_newline(self);
					auto arg_name = _glsl_name(self, arg.names[i].str);
					mn::print_to(self.out, "{};", _glsl_write_field(self, arg_type, arg_name).ptr);

					if (arg_type->kind == Type::KIND_STRUCT)
					{
						for (const auto& field: arg_type->struct_type.fields)
						{
							_glsl_newline(self);
							mn::print_to(self.out, "{}.{} = {};", arg_name, field.name.str, self.input_names[input_index++]);
						}
					}
					else
					{
						_glsl_newline(self);
						mn::print_to(self.out, "{} = {};", arg_name, self.input_names[input_index++]);
					}
				}
				type_index += arg.names.count;
			}

			// handle function return
			auto return_type = type->func.return_type;
			if (return_type != type_void)
			{
				_glsl_newline(self);
				_glsl_newline(self);

				auto output_name = _glsl_tmp_name(self);
				mn::print_to(self.out, "{} = {}(", _glsl_write_field(self, return_type, output_name), _glsl_symbol_name(entry));
				size_t arg_index = 0;
				for (auto arg: decl->func_decl.args)
				{
					for (auto name: arg.names)
					{
						if (arg_index > 0)
							mn::print_to(self.out, ", ");
						auto arg_name = _glsl_name(self, name.str);
						mn::print_to(self.out, "{}", arg_name);
						++arg_index;
					}
				}
				mn::print_to(self.out, ");");

				// fill output type
				size_t output_index = 0;
				if (return_type->kind == Type::KIND_STRUCT)
				{
					for (const auto& field: return_type->struct_type.fields)
					{
						_glsl_newline(self);
						mn::print_to(self.out, "{} = {}.{};", self.output_names[output_index++], output_name, field.name.str);
					}
				}
				else
				{
					_glsl_newline(self);
					mn::print_to(self.out, "{} = {};", self.output_names[output_index++], output_name);
				}
			}
		}
		--self.indent;
		_glsl_newline(self);
		mn::print_to(self.out, "}}");
	}


	// API
	GLSL
	glsl_new(Unit_Package* unit, mn::Stream out)
	{
		GLSL self{};
		self.unit = unit;
		self.out = out;
		self.indent = 0;
		self.scope_stack = mn::buf_new<Scope*>();

		// push global scope as first entry in scope stack
		auto global_scope = self.unit->global_scope;
		assert(global_scope != nullptr);
		mn::buf_push(self.scope_stack, global_scope);

		constexpr auto keywords_count = sizeof(GLSL_KEYWORDS) / sizeof(*GLSL_KEYWORDS);
		for (size_t i = 0; i < keywords_count; ++i)
		{
			auto keyword = unit_intern(self.unit->parent_unit, GLSL_KEYWORDS[i]);
			mn::map_insert(self.reserved_to_alternative, keyword, (const char*)nullptr);
		}

		return self;
	}

	void
	glsl_free(GLSL& self)
	{
		mn::buf_free(self.scope_stack);
		mn::map_free(self.reserved_to_alternative);
		mn::map_free(self.symbol_to_names);
		destruct(self.input_names);
		destruct(self.output_names);
	}

	void
	glsl_expr_gen(GLSL& self, Expr* e)
	{
		if (e->in_parens)
			mn::print_to(self.out, "(");

		switch (e->kind)
		{
		case Expr::KIND_ATOM:
			_glsl_gen_atom_expr(self, e);
			break;
		case Expr::KIND_BINARY:
			_glsl_gen_binary_expr(self, e);
			break;
		case Expr::KIND_UNARY:
			_glsl_gen_unary_expr(self, e);
			break;
		case Expr::KIND_DOT:
			_glsl_gen_dot_expr(self, e);
			break;
		case Expr::KIND_INDEXED:
			_glsl_gen_indexed_expr(self, e);
			break;
		case Expr::KIND_CALL:
			_glsl_gen_call_expr(self, e);
			break;
		case Expr::KIND_CAST:
			_glsl_gen_cast_expr(self, e);
			break;
		case Expr::KIND_COMPLIT:
			_glsl_gen_complit_expr(self, e);
			break;
		default:
			assert(false && "unreachable");
			break;
		}

		if (e->in_parens)
			mn::print_to(self.out, ")");
	}

	void
	glsl_stmt_gen(GLSL& self, Stmt* s)
	{
		// handle compound literal expressions
		bool is_const = s->kind == Stmt::KIND_DECL && s->decl_stmt->kind == Decl::KIND_CONST;
		_glsl_rewrite_complits_in_stmt(self, s, is_const);

		switch (s->kind)
		{
		case Stmt::KIND_BREAK:
			_glsl_gen_break_stmt(self, s);
			break;
		case Stmt::KIND_CONTINUE:
			_glsl_gen_continue_stmt(self, s);
			break;
		case Stmt::KIND_RETURN:
			_glsl_gen_return_stmt(self, s);
			break;
		case Stmt::KIND_IF:
			_glsl_gen_if_stmt(self, s);
			break;
		case Stmt::KIND_FOR:
			_glsl_gen_for_stmt(self, s);
			break;
		case Stmt::KIND_ASSIGN:
			_glsl_gen_assign_stmt(self, s);
			break;
		case Stmt::KIND_EXPR:
			glsl_expr_gen(self, s->expr_stmt);
			break;
		case Stmt::KIND_BLOCK:
			_glsl_gen_block_stmt(self, s);
			break;
		case Stmt::KIND_DECL:
			_glsl_gen_decl_stmt(self, s);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	void
	glsl_gen(GLSL& self)
	{
		auto compilation_unit = self.unit->parent_unit;

		if (compilation_unit->mode != COMPILATION_MODE_LIBRARY)
		{
			mn::print_to(self.out, "#version 450");
			_glsl_newline(self);
		}

		switch (compilation_unit->mode)
		{
		case COMPILATION_MODE_LIBRARY:
			// do nothing
			break;
		case COMPILATION_MODE_VERTEX:
			_glsl_generate_vertex_shader_io(self, compilation_unit->entry_symbol);
			break;
		case COMPILATION_MODE_PIXEL:
			_glsl_generate_pixel_shader_io(self, compilation_unit->entry_symbol);
			break;
		default:
			assert(false && "unreachable");
			break;
		}

		bool last_symbol_was_generated = false;
		for (size_t i = 0; i < self.unit->reachable_symbols.count; ++i)
		{
			if (last_symbol_was_generated)
				_glsl_newline(self);

			auto pos = _glsl_buffer_position(self);
			_glsl_symbol_gen(self, self.unit->reachable_symbols[i], false);
			last_symbol_was_generated = _glsl_code_generated_after(self, pos);
		}

		// generate real entry function
		if (compilation_unit->mode != COMPILATION_MODE_LIBRARY)
		{
			_glsl_newline(self);
			_glsl_newline(self);
			_glsl_generate_main_func(self, compilation_unit->entry_symbol);
		}
	}
}