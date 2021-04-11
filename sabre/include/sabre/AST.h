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
	type_sign_atom_named_new(Tkn tkn)
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
	type_sign_new()
	{
		return Type_Sign{};
	}

	// frees a type signature
	inline static void
	type_sign_free(Type_Sign& self)
	{
		mn::buf_free(self.atoms);
	}

	inline static void
	destruct(Type_Sign& self)
	{
		type_sign_free(self);
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
		Pos pos;
		Rng rng;
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
	expr_binary_new(Expr* lhs, Tkn op, Expr* rhs)
	{
		auto self = mn::alloc_zerod<Expr>();
		self->kind = Expr::KIND_BINARY;
		self->binary.left = lhs;
		self->binary.op = op;
		self->binary.right = rhs;
		return self;
	}

	// creates a new cast expression
	inline static Expr*
	expr_cast_new(Expr* base, Type_Sign type)
	{
		auto self = mn::alloc_zerod<Expr>();
		self->kind = Expr::KIND_CAST;
		self->cast.base = base;
		self->cast.type = type;
		return self;
	}

	// creates a new unary expression
	inline static Expr*
	expr_unary_new(Tkn op, Expr* base)
	{
		auto self = mn::alloc_zerod<Expr>();
		self->kind = Expr::KIND_UNARY;
		self->unary.op = op;
		self->unary.base = base;
		return self;
	}

	// creates a new call expression
	inline static Expr*
	expr_call_new(Expr* base, mn::Buf<Expr*> args)
	{
		auto self = mn::alloc_zerod<Expr>();
		self->kind = Expr::KIND_CALL;
		self->call.base = base;
		self->call.args = args;
		return self;
	}

	// creates a new indexed expression
	inline static Expr*
	expr_indexed_new(Expr* base, Expr* index)
	{
		auto self = mn::alloc_zerod<Expr>();
		self->kind = Expr::KIND_INDEXED;
		self->indexed.base = base;
		self->indexed.index = index;
		return self;
	}

	// creates new dot access expression
	inline static Expr*
	expr_dot_new(Expr* lhs, Expr* rhs)
	{
		auto self = mn::alloc_zerod<Expr>();
		self->kind = Expr::KIND_DOT;
		self->dot.lhs = lhs;
		self->dot.rhs = rhs;
		return self;
	}

	// creates a new atom expression
	inline static Expr*
	expr_atom_new(Tkn atom)
	{
		auto self = mn::alloc_zerod<Expr>();
		self->kind = Expr::KIND_ATOM;
		self->atom = atom;
		return self;
	}
}