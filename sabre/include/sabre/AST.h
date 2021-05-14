#pragma once

#include "sabre/Tkn.h"

#include <mn/Buf.h>
#include <mn/Memory.h>

namespace sabre
{
	// represents a type signature atom
	struct Type_Sign_Atom
	{
		enum KIND
		{
			KIND_NAMED,
		};

		KIND kind;
		union
		{
			struct
			{
				Tkn type_name;
				// optional, used in case we reference types from imported packages
				Tkn package_name;
			} named;
		};
	};

	// creates a new type sign atom with the given token
	inline static Type_Sign_Atom
	type_sign_atom_named(Tkn type_name, Tkn package_name)
	{
		Type_Sign_Atom self{};
		self.kind = Type_Sign_Atom::KIND_NAMED;
		self.named.type_name = type_name;
		self.named.package_name = package_name;
		return self;
	}

	// represents a type signature
	struct Type_Sign
	{
		mn::Buf<Type_Sign_Atom> atoms;
	};

	// creates a new type signature
	inline static Type_Sign
	type_sign_new(mn::Allocator arena)
	{
		Type_Sign self{};
		self.atoms = mn::buf_with_allocator<Type_Sign_Atom>(arena);
		return self;
	}

	// pushes a new type sign atom to a type signature
	inline static void
	type_sign_push(Type_Sign& self, Type_Sign_Atom atom)
	{
		mn::buf_push(self.atoms, atom);
	}

	// returns the location of the given type signature
	inline static Location
	type_sign_location(const Type_Sign& self)
	{
		Location res{};
		for (auto atom: self.atoms)
		{
			assert(atom.kind == Type_Sign_Atom::KIND_NAMED);
			if (res.file == nullptr)
			{
				res = atom.named.type_name.loc;
			}
			else
			{
				res.rng.end = atom.named.type_name.loc.rng.end;
			}
		}
		return res;
	}

	// expression values, used for compile time expressions
	struct Expr_Value
	{
		enum KIND
		{
			KIND_NONE,
			KIND_BOOL,
			KIND_INT,
			KIND_DOUBLE
		};

		KIND kind;
		union
		{
			bool as_bool;
			int64_t as_int;
			double as_double;
		};
	};

	// creates a new bool expression value
	inline static Expr_Value
	expr_value_bool(bool v)
	{
		Expr_Value self{};
		self.kind = Expr_Value::KIND_BOOL;
		self.as_bool = v;
		return self;
	}

	// creates a new int expression value
	inline static Expr_Value
	expr_value_int(int64_t v)
	{
		Expr_Value self{};
		self.kind = Expr_Value::KIND_INT;
		self.as_int = v;
		return self;
	}

	// creates a new double expression value
	inline static Expr_Value
	expr_value_double(double v)
	{
		Expr_Value self{};
		self.kind = Expr_Value::KIND_DOUBLE;
		self.as_double = v;
		return self;
	}

	// performs a logical or between two booleans, returns none value if one of them is not a bool
	inline static Expr_Value
	expr_value_logic_or(Expr_Value a, Expr_Value b)
	{
		if (a.kind != Expr_Value::KIND_BOOL || b.kind != Expr_Value::KIND_BOOL)
			return Expr_Value{};

		return expr_value_bool(a.as_bool || b.as_bool);
	}

	// performs a logical and between two booleans, returns none value if one of them is not a bool
	inline static Expr_Value
	expr_value_logic_and(Expr_Value a, Expr_Value b)
	{
		if (a.kind != Expr_Value::KIND_BOOL || b.kind != Expr_Value::KIND_BOOL)
			return Expr_Value{};

		return expr_value_bool(a.as_bool && b.as_bool);
	}

	// performs a compare and returns a boolean with the compare result
	inline static Expr_Value
	expr_value_cmp(Expr_Value a, Tkn::KIND op, Expr_Value b)
	{
		int r = 0;
		if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_INT)
		{
			r = a.as_int - b.as_int;
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_DOUBLE)
		{
			if (a.as_double < b.as_double)
				r = -1;
			else if (a.as_double > b.as_double)
				r = 1;
			else
				r = 0;
		}
		else if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_DOUBLE)
		{
			double a_double = a.as_int;
			if (a_double < b.as_double)
				r = -1;
			else if (a_double > b.as_double)
				r = 1;
			else
				r = 0;
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_INT)
		{
			double b_double = b.as_double;
			if (a.as_double < b_double)
				r = -1;
			else if (a.as_double > b_double)
				r = 1;
			else
				r = 0;
		}
		else
		{
			return Expr_Value{};
		}

		switch (op)
		{
		case Tkn::KIND_LESS:
			return expr_value_bool(r < 0);
		case Tkn::KIND_GREATER:
			return expr_value_bool(r > 0);
		case Tkn::KIND_LESS_EQUAL:
			return expr_value_bool(r <= 0);
		case Tkn::KIND_GREATER_EQUAL:
			return expr_value_bool(r >= 0);
		case Tkn::KIND_EQUAL_EQUAL:
			return expr_value_bool(r == 0);
		case Tkn::KIND_NOT_EQUAL:
			return expr_value_bool(r != 0);
		default:
			assert(false && "unreachable");
			return Expr_Value{};
		}
	}

	// performs an addition between 2 numerical values
	inline static Expr_Value
	expr_value_add(Expr_Value a, Expr_Value b)
	{
		if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_INT)
		{
			return expr_value_int(a.as_int + b.as_int);
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_DOUBLE)
		{
			return expr_value_double(a.as_double + b.as_double);
		}
		else if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_DOUBLE)
		{
			double a_double = a.as_int;
			return expr_value_double(a_double + b.as_double);
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_INT)
		{
			double b_double = b.as_double;
			return expr_value_double(a.as_double + b_double);
		}
		else
		{
			return Expr_Value{};
		}
	}

	// performs a subtraction between 2 numerical values
	inline static Expr_Value
	expr_value_sub(Expr_Value a, Expr_Value b)
	{
		if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_INT)
		{
			return expr_value_int(a.as_int - b.as_int);
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_DOUBLE)
		{
			return expr_value_double(a.as_double - b.as_double);
		}
		else if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_DOUBLE)
		{
			double a_double = a.as_int;
			return expr_value_double(a_double - b.as_double);
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_INT)
		{
			double b_double = b.as_double;
			return expr_value_double(a.as_double - b_double);
		}
		else
		{
			return Expr_Value{};
		}
	}

	// performs a multiplication between 2 numerical values
	inline static Expr_Value
	expr_value_mul(Expr_Value a, Expr_Value b)
	{
		if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_INT)
		{
			return expr_value_int(a.as_int * b.as_int);
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_DOUBLE)
		{
			return expr_value_double(a.as_double * b.as_double);
		}
		else if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_DOUBLE)
		{
			double a_double = a.as_int;
			return expr_value_double(a_double * b.as_double);
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_INT)
		{
			double b_double = b.as_double;
			return expr_value_double(a.as_double * b_double);
		}
		else
		{
			return Expr_Value{};
		}
	}

	// performs a division between 2 numerical values
	inline static Expr_Value
	expr_value_div(Expr_Value a, Expr_Value b)
	{
		if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_INT)
		{
			return expr_value_int(a.as_int / b.as_int);
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_DOUBLE)
		{
			return expr_value_double(a.as_double / b.as_double);
		}
		else if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_DOUBLE)
		{
			double a_double = a.as_int;
			return expr_value_double(a_double / b.as_double);
		}
		else if (a.kind == Expr_Value::KIND_DOUBLE && b.kind == Expr_Value::KIND_INT)
		{
			double b_double = b.as_double;
			return expr_value_double(a.as_double / b_double);
		}
		else
		{
			return Expr_Value{};
		}
	}

	// performs a mod between 2 numerical values
	inline static Expr_Value
	expr_value_mod(Expr_Value a, Expr_Value b)
	{
		if (a.kind == Expr_Value::KIND_INT && b.kind == Expr_Value::KIND_INT)
		{
			return expr_value_int(a.as_int % b.as_int);
		}
		else
		{
			return Expr_Value{};
		}
	}

	// performs a binary operation between 2 expression values
	inline static Expr_Value
	expr_value_binar_op(Expr_Value a, Tkn::KIND op, Expr_Value b)
	{
		switch (op)
		{
		case Tkn::KIND_LOGICAL_OR:
			return expr_value_logic_or(a, b);
		case Tkn::KIND_LOGICAL_AND:
			return expr_value_logic_and(a, b);
		case Tkn::KIND_LESS:
		case Tkn::KIND_GREATER:
		case Tkn::KIND_LESS_EQUAL:
		case Tkn::KIND_GREATER_EQUAL:
		case Tkn::KIND_EQUAL_EQUAL:
		case Tkn::KIND_NOT_EQUAL:
			return expr_value_cmp(a, op, b);
		case Tkn::KIND_PLUS:
			return expr_value_add(a, b);
		case Tkn::KIND_MINUS:
			return expr_value_sub(a, b);
		case Tkn::KIND_STAR:
			return expr_value_mul(a, b);
		case Tkn::KIND_DIVIDE:
			return expr_value_div(a, b);
		case Tkn::KIND_MODULUS:
			return expr_value_mod(a, b);
		default:
			assert(false && "unreachable");
			return Expr_Value{};
		}
	}

	// performs an unary operation on an expression value
	inline static Expr_Value
	expr_value_unary_op(Expr_Value a, Tkn::KIND op)
	{
		switch (op)
		{
		case Tkn::KIND_INC:
			if (a.kind == Expr_Value::KIND_INT)
				return expr_value_int(a.as_int + 1);
			else if (a.kind == Expr_Value::KIND_DOUBLE)
				return expr_value_double(a.as_double + 1);
			else
				return Expr_Value{};
		case Tkn::KIND_DEC:
			if (a.kind == Expr_Value::KIND_INT)
				return expr_value_int(a.as_int - 1);
			else if (a.kind == Expr_Value::KIND_DOUBLE)
				return expr_value_double(a.as_double - 1);
			else
				return Expr_Value{};
		case Tkn::KIND_LOGICAL_NOT:
			if (a.kind == Expr_Value::KIND_BOOL)
				return expr_value_bool(!a.as_bool);
			else
				return Expr_Value{};
		case Tkn::KIND_PLUS:
			return a;
		case Tkn::KIND_MINUS:
			if (a.kind == Expr_Value::KIND_INT)
				return expr_value_int(-a.as_int);
			else if (a.kind == Expr_Value::KIND_DOUBLE)
				return expr_value_double(-a.as_double);
			else
				return Expr_Value{};
		default:
			assert(false && "unreachable");
			return Expr_Value{};
		}
	}

	struct Type;

	// address mode of expressions which is used to check assignment statements, etc...
	enum ADDRESS_MODE
	{
		ADDRESS_MODE_NONE,
		// expression is constant
		ADDRESS_MODE_CONST,
		// expression is a function call like
		ADDRESS_MODE_COMPUTED_VALUE,
		// expression is a variable (something with an address)
		ADDRESS_MODE_VARIABLE,
	};

	// represents an expression
	struct Expr
	{
		enum KIND
		{
			KIND_ATOM,
			KIND_BINARY,
			KIND_UNARY,
			KIND_CALL,
			KIND_CAST,
			KIND_DOT,
			KIND_INDEXED,
		};

		KIND kind;
		Location loc;
		Type* type;
		bool in_parens;
		ADDRESS_MODE mode;
		Expr_Value const_value;

		union
		{
			Tkn atom;

			struct
			{
				Expr* left;
				Tkn op;
				Expr* right;
			} binary;

			struct
			{
				Tkn op;
				Expr* base;
			} unary;

			struct
			{
				Expr* base;
				mn::Buf<Expr*> args;
			} call;

			struct
			{
				Expr* base;
				Type_Sign type;
			} cast;

			struct
			{
				Expr* lhs;
				Expr* rhs;
			} dot;

			struct
			{
				Expr* base;
				Expr* index;
			} indexed;
		};
	};

	// creates a new binary expression
	inline static Expr*
	expr_binary_new(mn::Allocator arena, Expr* lhs, Tkn op, Expr* rhs)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_BINARY;
		self->binary.left = lhs;
		self->binary.op = op;
		self->binary.right = rhs;
		return self;
	}

	// creates a new cast expression
	inline static Expr*
	expr_cast_new(mn::Allocator arena, Expr* base, Type_Sign type)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_CAST;
		self->cast.base = base;
		self->cast.type = type;
		return self;
	}

	// creates a new unary expression
	inline static Expr*
	expr_unary_new(mn::Allocator arena, Tkn op, Expr* base)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_UNARY;
		self->unary.op = op;
		self->unary.base = base;
		return self;
	}

	// creates a new call expression
	inline static Expr*
	expr_call_new(mn::Allocator arena, Expr* base, mn::Buf<Expr*> args)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_CALL;
		self->call.base = base;
		self->call.args = args;
		return self;
	}

	// creates a new indexed expression
	inline static Expr*
	expr_indexed_new(mn::Allocator arena, Expr* base, Expr* index)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_INDEXED;
		self->indexed.base = base;
		self->indexed.index = index;
		return self;
	}

	// creates new dot access expression
	inline static Expr*
	expr_dot_new(mn::Allocator arena, Expr* lhs, Expr* rhs)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_DOT;
		self->dot.lhs = lhs;
		self->dot.rhs = rhs;
		return self;
	}

	// creates a new atom expression
	inline static Expr*
	expr_atom_new(mn::Allocator arena, Tkn atom)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_ATOM;
		self->atom = atom;
		return self;
	}

	struct Decl;

	// Stmt
	struct Stmt
	{
		enum KIND
		{
			KIND_BREAK,
			KIND_CONTINUE,
			KIND_RETURN,
			KIND_IF,
			KIND_FOR,
			KIND_ASSIGN,
			KIND_EXPR,
			KIND_BLOCK,
			KIND_DECL,
		};

		KIND kind;
		Location loc;
		union
		{
			Tkn break_stmt;

			Tkn continue_stmt;

			Expr* return_stmt;

			struct
			{
				mn::Buf<Expr*> cond;
				mn::Buf<Stmt*> body;
				Stmt* else_body;
			} if_stmt;

			struct
			{
				Stmt* init;
				Expr* cond;
				Stmt* post;
				Stmt* body;
			} for_stmt;

			struct
			{
				mn::Buf<Expr*> lhs;
				mn::Buf<Expr*> rhs;
				Tkn op;
			} assign_stmt;

			Expr* expr_stmt;

			mn::Buf<Stmt*> block_stmt;

			Decl* decl_stmt;
		};
	};

	// creates a new break stmt
	inline static Stmt*
	stmt_break_new(mn::Allocator arena, Tkn tkn)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_BREAK;
		self->break_stmt = tkn;
		return self;
	}

	// creates a new continue stmt
	inline static Stmt*
	stmt_continue_new(mn::Allocator arena, Tkn tkn)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_CONTINUE;
		self->continue_stmt = tkn;
		return self;
	}

	// creates a new return stmt
	inline static Stmt*
	stmt_return_new(mn::Allocator arena, Expr* expr)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_RETURN;
		self->return_stmt = expr;
		return self;
	}

	// creates a new block statement
	inline static Stmt*
	stmt_block_new(mn::Allocator arena, mn::Buf<Stmt*> stmts)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_BLOCK;
		self->block_stmt = stmts;
		return self;
	}

	// creates a new if statement
	inline static Stmt*
	stmt_if_new(mn::Allocator arena, mn::Buf<Expr*> cond, mn::Buf<Stmt*> body, Stmt* else_body)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_IF;
		self->if_stmt.cond = cond;
		self->if_stmt.body = body;
		self->if_stmt.else_body = else_body;
		return self;
	}

	// creates a new for statement
	inline static Stmt*
	stmt_for_new(mn::Allocator arena, Stmt* init, Expr* cond, Stmt* post, Stmt* body)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_FOR;
		self->for_stmt.init = init;
		self->for_stmt.cond = cond;
		self->for_stmt.post = post;
		self->for_stmt.body = body;
		return self;
	}

	// creates a new assigment statement
	inline static Stmt*
	stmt_assign_new(mn::Allocator arena, mn::Buf<Expr*> lhs, Tkn op, mn::Buf<Expr*> rhs)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_ASSIGN;
		self->assign_stmt.lhs = lhs;
		self->assign_stmt.op = op;
		self->assign_stmt.rhs = rhs;
		return self;
	}

	// creates a new expression statement
	inline static Stmt*
	stmt_expr_new(mn::Allocator arena, Expr* expr)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_EXPR;
		self->expr_stmt = expr;
		return self;
	}

	// creates a new declaration statement
	inline static Stmt*
	stmt_decl_new(mn::Allocator arena, Decl* decl)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_DECL;
		self->decl_stmt = decl;
		return self;
	}

	// Hasher for Tkn keys
	struct Tkn_Hasher
	{
		inline size_t
		operator()(const Tkn& tkn) const
		{
			return (size_t)tkn.str;
		}
	};

	// represents a tag that can be attached to declarations
	struct Tag
	{
		Tkn name;
		mn::Map<Tkn, Tkn, Tkn_Hasher> args;
	};

	// create a new tag
	inline static Tag
	tag_new(mn::Allocator arena)
	{
		Tag self{};
		self.args = mn::map_with_allocator<Tkn, Tkn, Tkn_Hasher>(arena);
		return self;
	}

	// represents a set of tags attached to a declaration
	struct Tag_Table
	{
		mn::Map<Tkn, Tag, Tkn_Hasher> table;
	};

	// creates a new tag table
	inline static Tag_Table
	tag_table_new(mn::Allocator arena)
	{
		Tag_Table self{};
		self.table = mn::map_with_allocator<Tkn, Tag, Tkn_Hasher>(arena);
		return self;
	}

	struct Arg
	{
		Tag_Table tags;
		mn::Buf<Tkn> names;
		Type_Sign type;
	};

	// creates a new argument
	inline static Arg
	arg_new(mn::Allocator arena)
	{
		Arg self{};
		self.tags = tag_table_new(arena);
		self.names = mn::buf_with_allocator<Tkn>(arena);
		self.type = type_sign_new(arena);
		return self;
	}

	struct Field
	{
		Tag_Table tags;
		mn::Buf<Tkn> names;
		Type_Sign type;
	};

	inline static Field
	field_new(mn::Allocator arena)
	{
		Field self{};
		self.tags = tag_table_new(arena);
		self.names = mn::buf_with_allocator<Tkn>(arena);
		self.type = type_sign_new(arena);
		return self;
	}

	// Decl
	struct Decl
	{
		enum KIND
		{
			KIND_CONST,
			KIND_VAR,
			KIND_FUNC,
			KIND_STRUCT,
			KIND_IMPORT,
			KIND_IF,
		};

		KIND kind;
		Location loc;
		Tkn name;
		Tag_Table tags;
		union
		{
			struct
			{
				mn::Buf<Tkn> names;
				mn::Buf<Expr*> values;
				Type_Sign type;
			} const_decl;

			struct
			{
				mn::Buf<Tkn> names;
				mn::Buf<Expr*> values;
				Type_Sign type;
			} var_decl;

			struct
			{
				mn::Buf<Arg> args;
				Type_Sign return_type;
				Stmt* body;
			} func_decl;

			struct
			{
				mn::Buf<Field> fields;
			} struct_decl;

			struct
			{
				Tkn path;
				Tkn name;
			} import_decl;

			struct
			{
				mn::Buf<Expr*> cond;
				mn::Buf<mn::Buf<Decl*>> body;
				mn::Buf<Decl*> else_body;
			} if_decl;
		};
	};

	// creates a new variable declaration
	inline static Decl*
	decl_var_new(mn::Allocator arena, mn::Buf<Tkn> names, mn::Buf<Expr*> values, Type_Sign type)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_VAR;
		self->var_decl.names = names;
		self->var_decl.values = values;
		self->var_decl.type = type;
		return self;
	}

	// converts a given variable declaration to constant
	inline static Decl*
	decl_convert_var_to_const(Decl* var)
	{
		auto self = var;
		self->kind = Decl::KIND_CONST;
		self->const_decl.names = var->var_decl.names;
		self->const_decl.values = var->var_decl.values;
		self->const_decl.type = var->var_decl.type;
		return self;
	}

	// creates a new function declaration
	inline static Decl*
	decl_func_new(mn::Allocator arena, Tkn name, mn::Buf<Arg> args, Type_Sign ret, Stmt* body)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_FUNC;
		self->name = name;
		self->func_decl.args = args;
		self->func_decl.return_type = ret;
		self->func_decl.body = body;
		return self;
	}

	// creates a new struct declaration
	inline static Decl*
	decl_struct_new(mn::Allocator arena, Tkn name, mn::Buf<Field> fields)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_STRUCT;
		self->name = name;
		self->struct_decl.fields = fields;
		return self;
	}

	// creates a new import declaration
	inline static Decl*
	decl_import_new(mn::Allocator arena, Tkn path, Tkn name)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_IMPORT;
		self->name = name;
		self->import_decl.path = path;
		self->import_decl.name = name;
		return self;
	}

	// creates a new if delcaration
	inline static Decl*
	decl_if_new(mn::Allocator arena, mn::Buf<Expr*> cond, mn::Buf<mn::Buf<Decl*>> body, mn::Buf<Decl*> else_body)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_IF;
		self->if_decl.cond = cond;
		self->if_decl.body = body;
		self->if_decl.else_body = else_body;
		return self;
	}
}