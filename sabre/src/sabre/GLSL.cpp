#include "sabre/GLSL.h"
#include "sabre/Unit.h"
#include "sabre/Type_Interner.h"
#include "sabre/AST.h"

namespace sabre
{
	inline static void
	_glsl_write_field(GLSL& self, Type* type, const mn::Str& name)
	{
		switch (type->kind)
		{
		case Type::KIND_VOID:
			mn::print_to(self.out, "void");
			break;
		case Type::KIND_BOOL:
			mn::print_to(self.out, "bool");
			break;
		case Type::KIND_INT:
			mn::print_to(self.out, "int");
			break;
		case Type::KIND_FLOAT:
			mn::print_to(self.out, "float");
			break;
		case Type::KIND_DOUBLE:
			mn::print_to(self.out, "double");
			break;
		case Type::KIND_VEC:
			if (type->vec.base == type_bool)
			{
				switch(type->vec.width)
				{
				case 2:
					mn::print_to(self.out, "bvec2");
					break;
				case 3:
					mn::print_to(self.out, "bvec3");
					break;
				case 4:
					mn::print_to(self.out, "bvec4");
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
					mn::print_to(self.out, "ivec2");
					break;
				case 3:
					mn::print_to(self.out, "ivec3");
					break;
				case 4:
					mn::print_to(self.out, "ivec4");
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
					mn::print_to(self.out, "uvec2");
					break;
				case 3:
					mn::print_to(self.out, "uvec3");
					break;
				case 4:
					mn::print_to(self.out, "uvec4");
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
					mn::print_to(self.out, "vec2");
					break;
				case 3:
					mn::print_to(self.out, "vec3");
					break;
				case 4:
					mn::print_to(self.out, "vec4");
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
					mn::print_to(self.out, "dvec2");
					break;
				case 3:
					mn::print_to(self.out, "dvec3");
					break;
				case 4:
					mn::print_to(self.out, "dvec4");
					break;
				default:
					assert(false && "unreachable");
					break;
				}
			}
			break;
		default:
			assert(false && "unreachable");
			break;
		}
	}

	inline static void
	_glsl_write_field(GLSL& self, Type* type, const char* name)
	{
		return _glsl_write_field(self, type, mn::str_lit(name));
	}

	inline static void
	_glsl_gen_atom_expr(GLSL& self, Expr* e)
	{
		mn::print_to(self.out, e->atom.str);
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
		glsl_expr_gen(self, e->dot.lhs);
		mn::print_to(self.out, ".");
		glsl_expr_gen(self, e->dot.rhs);
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
		_glsl_write_field(self, e->type, nullptr);
		mn::print_to(self.out, "(");
		glsl_expr_gen(self, e->cast.base);
		mn::print_to(self.out, ")");
	}

	// API
	GLSL
	glsl_new(Unit* unit, mn::Stream out)
	{
		GLSL self{};
		self.unit = unit;
		self.out = out;
		self.indent = 0;
		self.scope_stack = mn::buf_new<Scope*>();

		// push global scope as first entry in scope stack
		auto global_scope = unit_scope_find(self.unit, nullptr);
		assert(global_scope != nullptr);
		mn::buf_push(self.scope_stack, global_scope);

		return self;
	}

	void
	glsl_free(GLSL& self)
	{
		mn::buf_free(self.scope_stack);
	}

	void
	glsl_expr_gen(GLSL& self, Expr* e)
	{
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
		default:
			assert(false && "unreachable");
			break;
		}
	}
}