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
			Tkn named;
		};
	};

	// creates a new type sign atom with the given token
	inline static Type_Sign_Atom
	type_sign_atom_named(Tkn tkn)
	{
		Type_Sign_Atom self{};
		self.kind = Type_Sign_Atom::KIND_NAMED;
		self.named = tkn;
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
		bool in_parens;

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

	struct Arg
	{
		mn::Buf<Tkn> names;
		Type_Sign type;
	};

	// creates a new argument
	inline static Arg
	arg_new(mn::Allocator arena)
	{
		Arg self{};
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
		};

		KIND kind;
		Location loc;
		Tkn name;
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
}