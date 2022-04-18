#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"
#include "sabre/Expr_Value.h"

#include <mn/Buf.h>
#include <mn/Memory.h>

namespace sabre
{
	struct Expr;
	struct Decl;
	struct Type_Sign;
	struct Type;
	struct Symbol;

	// represents a type signature atom
	struct Type_Sign_Atom
	{
		enum KIND
		{
			KIND_NAMED,
			KIND_ARRAY,
			KIND_TEMPLATED,
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

			struct
			{
				// optional, if the user specifies the size of the array manually
				Expr* static_size;
			} array;

			struct
			{
				Tkn type_name;
				// optional, used in case we reference types from imported packages
				Tkn package_name;
				mn::Buf<Type_Sign> args;
			} templated;
		};
	};

	// creates a new type sign atom with the given token
	SABRE_EXPORT Type_Sign_Atom
	type_sign_atom_named(Tkn type_name, Tkn package_name);

	// creates a new type sign array with the given size tkn
	SABRE_EXPORT Type_Sign_Atom
	type_sign_atom_array(Expr* static_size);

	// creates a new templated type sign atom with the given name and arguments
	SABRE_EXPORT Type_Sign_Atom
	type_sign_atom_templated(Tkn type_name, Tkn package_name, mn::Buf<Type_Sign> args);

	// copies the given type signature atom
	SABRE_EXPORT Type_Sign_Atom
	type_sign_atom_clone(const Type_Sign_Atom& other);

	inline static Type_Sign_Atom
	clone(const Type_Sign_Atom& other)
	{
		return type_sign_atom_clone(other);
	}

	// represents a type signature
	struct Type_Sign
	{
		mn::Buf<Type_Sign_Atom> atoms;
		Location loc;
	};

	// creates a new type signature
	SABRE_EXPORT Type_Sign
	type_sign_new(mn::Allocator arena);

	// pushes a new type sign atom to a type signature
	SABRE_EXPORT void
	type_sign_push(Type_Sign& self, Type_Sign_Atom atom);

	// returns the location of the given type signature
	SABRE_EXPORT Location
	type_sign_location(const Type_Sign& self);

	// copies the given type signature
	SABRE_EXPORT Type_Sign
	type_sign_clone(const Type_Sign& other, mn::Allocator arena);

	// clone overload for type sign
	inline static Type_Sign
	clone(const Type_Sign& other)
	{
		return type_sign_clone(other, other.atoms.allocator);
	}

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

	// represents a compound literal field
	struct Complit_Field
	{
		// the name which the user has written in complit expression
		Expr* selector_name;
		// index of said field in the type system (this is filled by the type checker)
		size_t selector_index;
		// value assigned to that field
		Expr* value;
	};

	// copies the given complit field
	SABRE_EXPORT Complit_Field
	complit_field_clone(const Complit_Field& other);

	// clone overload for the complit field
	inline static Complit_Field
	clone(const Complit_Field& other)
	{
		return complit_field_clone(other);
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
			KIND_COMPLIT,
		};

		KIND kind;
		// arena used to allocate this expression
		mn::Allocator arena;
		Location loc;
		Type* type;
		bool in_parens;
		ADDRESS_MODE mode;
		Expr_Value const_value;
		// symbol which this expression refer to, this can be null if it doesn't refer to a symbol
		Symbol* symbol;

		union
		{
			struct
			{
				Tkn tkn;
				// the exact declaration which this atom uses
				Decl* decl;
			} atom;

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
				// the actual function which this call uses
				Decl* func;
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

			struct
			{
				Type_Sign type;
				mn::Buf<Complit_Field> fields;
				// map of field index in type system (Type) to field index in the above fields buf
				// this is used to check if the user has referenced the same field twice
				mn::Map<size_t, size_t> referenced_fields;
			} complit;
		};
	};

	// creates a new binary expression
	SABRE_EXPORT Expr*
	expr_binary_new(mn::Allocator arena, Expr* lhs, Tkn op, Expr* rhs);

	// creates a new cast expression
	SABRE_EXPORT Expr*
	expr_cast_new(mn::Allocator arena, Expr* base, Type_Sign type);

	// creates a new unary expression
	SABRE_EXPORT Expr*
	expr_unary_new(mn::Allocator arena, Tkn op, Expr* base);

	// creates a new call expression
	SABRE_EXPORT Expr*
	expr_call_new(mn::Allocator arena, Expr* base, mn::Buf<Expr*> args);

	// creates a new indexed expression
	SABRE_EXPORT Expr*
	expr_indexed_new(mn::Allocator arena, Expr* base, Expr* index);

	// creates new dot access expression
	SABRE_EXPORT Expr*
	expr_dot_new(mn::Allocator arena, Expr* lhs, Expr* rhs);

	// creates a new atom expression
	SABRE_EXPORT Expr*
	expr_atom_new(mn::Allocator arena, Tkn atom);

	// creates a new compound literal expression
	SABRE_EXPORT Expr*
	expr_complit_new(mn::Allocator arena, Type_Sign type, mn::Buf<Complit_Field> fields);

	// duplicates the given expr node
	SABRE_EXPORT Expr*
	expr_clone(const Expr* other, mn::Allocator arena);

	// clone overload for the expression node
	inline static Expr*
	clone(const Expr* e)
	{
		if (e == nullptr)
			return nullptr;

		return expr_clone(e, e->arena);
	}

	// Stmt
	struct Stmt
	{
		enum KIND
		{
			KIND_BREAK,
			KIND_CONTINUE,
			KIND_DISCARD,
			KIND_RETURN,
			KIND_IF,
			KIND_FOR,
			KIND_ASSIGN,
			KIND_EXPR,
			KIND_BLOCK,
			KIND_DECL,
		};

		KIND kind;
		mn::Allocator arena;
		Location loc;
		union
		{
			Tkn break_stmt;

			Tkn continue_stmt;

			Tkn discard_stmt;

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
	SABRE_EXPORT Stmt*
	stmt_break_new(mn::Allocator arena, Tkn tkn);

	// creates a new continue stmt
	SABRE_EXPORT Stmt*
	stmt_continue_new(mn::Allocator arena, Tkn tkn);

	// creates a new discard stmt
	SABRE_EXPORT Stmt*
	stmt_discard_new(mn::Allocator arena, Tkn tkn);

	// creates a new return stmt
	SABRE_EXPORT Stmt*
	stmt_return_new(mn::Allocator arena, Expr* expr);

	// creates a new block statement
	SABRE_EXPORT Stmt*
	stmt_block_new(mn::Allocator arena, mn::Buf<Stmt*> stmts);

	// creates a new if statement
	SABRE_EXPORT Stmt*
	stmt_if_new(mn::Allocator arena, mn::Buf<Expr*> cond, mn::Buf<Stmt*> body, Stmt* else_body);

	// creates a new for statement
	SABRE_EXPORT Stmt*
	stmt_for_new(mn::Allocator arena, Stmt* init, Expr* cond, Stmt* post, Stmt* body);

	// creates a new assignment statement
	SABRE_EXPORT Stmt*
	stmt_assign_new(mn::Allocator arena, mn::Buf<Expr*> lhs, Tkn op, mn::Buf<Expr*> rhs);

	// creates a new expression statement
	SABRE_EXPORT Stmt*
	stmt_expr_new(mn::Allocator arena, Expr* expr);

	// creates a new declaration statement
	SABRE_EXPORT Stmt*
	stmt_decl_new(mn::Allocator arena, Decl* decl);

	// copies the given stmt
	SABRE_EXPORT Stmt*
	stmt_clone(const Stmt* other, mn::Allocator arena);

	// clone overload for the stmt clone
	inline static Stmt*
	clone(const Stmt* other)
	{
		if (other == nullptr)
			return nullptr;

		return stmt_clone(other, other->arena);
	}

	struct Tag_Key_Value
	{
		Tkn key;
		Expr* value;
	};

	// represents a tag that can be attached to declarations
	struct Tag
	{
		Tkn name;
		mn::Map<const char*, Tag_Key_Value> args;
	};

	// create a new tag
	SABRE_EXPORT Tag
	tag_new(mn::Allocator arena);

	// represents a set of tags attached to a declaration
	struct Tag_Table
	{
		mn::Map<const char*, Tag> table;
	};

	// creates a new tag table
	SABRE_EXPORT Tag_Table
	tag_table_new(mn::Allocator arena);

	struct Arg
	{
		Tag_Table tags;
		mn::Buf<Tkn> names;
		Type_Sign type;
	};

	// creates a new argument
	SABRE_EXPORT Arg
	arg_new(mn::Allocator arena);

	// copies the arg
	SABRE_EXPORT Arg
	arg_clone(const Arg& other, mn::Allocator arena);

	// clone overload for arg
	inline static Arg
	clone(const Arg& other)
	{
		return arg_clone(other, other.names.allocator);
	}

	struct Field
	{
		Tag_Table tags;
		mn::Buf<Tkn> names;
		Type_Sign type;
		Expr* default_value;
	};

	// creates a new empty field instance
	SABRE_EXPORT Field
	field_new(mn::Allocator arena);

	// copies the given field
	SABRE_EXPORT Field
	field_clone(const Field& other, mn::Allocator arena);

	// clone overload for field
	inline static Field
	clone(const Field& other)
	{
		return field_clone(other, other.names.allocator);
	}

	struct Enum_Field
	{
		Tkn name;
		Expr* value;
	};

	// copies the given enum field
	SABRE_EXPORT Enum_Field
	enum_field_clone(const Enum_Field& other);

	// clone overload for enum field
	inline static Enum_Field
	clone(const Enum_Field& other)
	{
		return enum_field_clone(other);
	}

	struct Texture_Sample_Operand
	{
		enum KIND
		{
			KIND_ARG,
			KIND_GLOBAL,
		};

		KIND kind;
		union
		{
			size_t arg_index;
			Symbol* global_sym;
		};
	};

	// represents a texture sample operation, this is used to associate combined texture sampler for opengl level glsl
	struct Texture_Sample_Op
	{
		Texture_Sample_Operand texture;
		Texture_Sample_Operand sampler;
	};

	struct Template_Arg
	{
		mn::Buf<Tkn> names;
		Type_Sign default_type;
	};

	// copies the template arguments list
	SABRE_EXPORT Template_Arg
	template_arg_clone(const Template_Arg& other, mn::Allocator arena);

	// clone overload for template arg
	inline static Template_Arg
	clone(const Template_Arg& other)
	{
		return template_arg_clone(other, other.names.allocator);
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
			KIND_ENUM,
		};

		KIND kind;
		mn::Allocator arena;
		Location loc;
		Tkn name;
		Tag_Table tags;
		Type* type;
		mn::Buf<Template_Arg> template_args;
		Symbol* symbol;
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

			struct
			{
				mn::Buf<Enum_Field> fields;
			} enum_decl;
		};
	};

	// creates a new variable declaration
	SABRE_EXPORT Decl*
	decl_var_new(mn::Allocator arena, mn::Buf<Tkn> names, mn::Buf<Expr*> values, Type_Sign type);

	// converts a given variable declaration to constant
	SABRE_EXPORT Decl*
	decl_convert_var_to_const(Decl* var);

	// creates a new function declaration
	SABRE_EXPORT Decl*
	decl_func_new(mn::Allocator arena, Tkn name, mn::Buf<Arg> args, Type_Sign ret, Stmt* body, mn::Buf<Template_Arg> template_args);

	// creates a new struct declaration
	SABRE_EXPORT Decl*
	decl_struct_new(mn::Allocator arena, Tkn name, mn::Buf<Field> fields, mn::Buf<Template_Arg> template_args);

	// creates a new import declaration
	SABRE_EXPORT Decl*
	decl_import_new(mn::Allocator arena, Tkn path, Tkn name);

	// creates a new if delcaration
	SABRE_EXPORT Decl*
	decl_if_new(mn::Allocator arena, mn::Buf<Expr*> cond, mn::Buf<mn::Buf<Decl*>> body, mn::Buf<Decl*> else_body);

	// creates a new enum declaration
	SABRE_EXPORT Decl*
	decl_enum_new(mn::Allocator arena, Tkn name, mn::Buf<Enum_Field> fields);

	// copies the given declaration instance
	SABRE_EXPORT Decl*
	decl_clone(const Decl* other, mn::Allocator arena);

	inline static Decl*
	clone(const Decl* other)
	{
		if (other == nullptr)
			return nullptr;

		return decl_clone(other, other->arena);
	}
}