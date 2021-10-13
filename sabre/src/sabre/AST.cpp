#include "sabre/AST.h"

namespace sabre
{
	// API
	Type_Sign_Atom
	type_sign_atom_named(Tkn type_name, Tkn package_name)
	{
		Type_Sign_Atom self{};
		self.kind = Type_Sign_Atom::KIND_NAMED;
		self.named.type_name = type_name;
		self.named.package_name = package_name;
		return self;
	}

	Type_Sign_Atom
	type_sign_atom_array(Expr* static_size)
	{
		Type_Sign_Atom self{};
		self.kind = Type_Sign_Atom::KIND_ARRAY;
		self.array.static_size = static_size;
		return self;
	}

	Type_Sign_Atom
	type_sign_atom_templated(Tkn type_name, Tkn package_name, mn::Buf<Type_Sign> args)
	{
		Type_Sign_Atom self{};
		self.kind = Type_Sign_Atom::KIND_TEMPLATED;
		self.templated.type_name = type_name;
		self.templated.package_name = package_name;
		self.templated.args = args;
		return self;
	}

	Type_Sign_Atom
	type_sign_atom_clone(const Type_Sign_Atom& other)
	{
		auto self = other;
		switch (self.kind)
		{
		case Type_Sign_Atom::KIND_NAMED:
			// do nothing
			break;
		case Type_Sign_Atom::KIND_ARRAY:
			self.array.static_size = clone(other.array.static_size);
			break;
		case Type_Sign_Atom::KIND_TEMPLATED:
			self.templated.args = clone(other.templated.args);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
		return self;
	}

	Type_Sign
	type_sign_new(mn::Allocator arena)
	{
		Type_Sign self{};
		self.atoms = mn::buf_with_allocator<Type_Sign_Atom>(arena);
		return self;
	}

	void
	type_sign_push(Type_Sign& self, Type_Sign_Atom atom)
	{
		mn::buf_push(self.atoms, atom);
	}

	Location
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

	Type_Sign
	type_sign_clone(const Type_Sign& other, mn::Allocator arena)
	{
		return {mn::buf_clone(other.atoms, arena)};
	}

	Complit_Field
	complit_field_clone(const Complit_Field& other)
	{
		auto self = other;
		self.selector_name = clone(other.selector_name);
		self.value = clone(other.value);
		return self;
	}

	Expr*
	expr_binary_new(mn::Allocator arena, Expr* lhs, Tkn op, Expr* rhs)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_BINARY;
		self->binary.left = lhs;
		self->binary.op = op;
		self->binary.right = rhs;
		return self;
	}

	Expr*
	expr_cast_new(mn::Allocator arena, Expr* base, Type_Sign type)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_CAST;
		self->cast.base = base;
		self->cast.type = type;
		return self;
	}

	Expr*
	expr_unary_new(mn::Allocator arena, Tkn op, Expr* base)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_UNARY;
		self->unary.op = op;
		self->unary.base = base;
		return self;
	}

	Expr*
	expr_call_new(mn::Allocator arena, Expr* base, mn::Buf<Expr*> args)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_CALL;
		self->call.base = base;
		self->call.args = args;
		return self;
	}

	Expr*
	expr_indexed_new(mn::Allocator arena, Expr* base, Expr* index)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_INDEXED;
		self->indexed.base = base;
		self->indexed.index = index;
		return self;
	}

	Expr*
	expr_dot_new(mn::Allocator arena, Expr* lhs, Expr* rhs)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_DOT;
		self->dot.lhs = lhs;
		self->dot.rhs = rhs;
		return self;
	}

	Expr*
	expr_atom_new(mn::Allocator arena, Tkn atom)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_ATOM;
		self->atom.tkn = atom;
		return self;
	}

	Expr*
	expr_complit_new(mn::Allocator arena, Type_Sign type, mn::Buf<Complit_Field> fields)
	{
		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = Expr::KIND_COMPLIT;
		self->complit.type = type;
		self->complit.fields = fields;
		self->complit.referenced_fields = mn::map_with_allocator<size_t, size_t>(arena);
		return self;
	}

	Expr*
	expr_clone(const Expr* other, mn::Allocator arena)
	{
		if (other == nullptr)
			return nullptr;

		auto self = mn::alloc_zerod_from<Expr>(arena);
		self->kind = other->kind;
		self->arena = arena;
		self->loc = other->loc;
		self->in_parens = other->in_parens;
		switch (other->kind)
		{
		case Expr::KIND_ATOM:
			self->atom.tkn = other->atom.tkn;
			break;
		case Expr::KIND_BINARY:
			self->binary.left = clone(other->binary.left);
			self->binary.op = other->binary.op;
			self->binary.right = clone(other->binary.right);
			break;
		case Expr::KIND_UNARY:
			self->unary.op = other->unary.op;
			self->unary.base = clone(other->unary.base);
			break;
		case Expr::KIND_CALL:
			self->call.base = clone(other->call.base);
			self->call.args = mn::buf_clone(other->call.args, arena);
			break;
		case Expr::KIND_CAST:
			self->cast.base = clone(other->cast.base);
			self->cast.type = clone(other->cast.type);
			break;
		case Expr::KIND_DOT:
			self->dot.lhs = clone(other->dot.lhs);
			self->dot.rhs = clone(other->dot.rhs);
			break;
		case Expr::KIND_INDEXED:
			self->indexed.base = clone(other->indexed.base);
			self->indexed.index = clone(other->indexed.index);
			break;
		case Expr::KIND_COMPLIT:
			self->complit.type = clone(other->complit.type);
			self->complit.fields = mn::buf_clone(other->complit.fields, arena);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
		return self;
	}

	Stmt*
	stmt_break_new(mn::Allocator arena, Tkn tkn)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_BREAK;
		self->arena = arena;
		self->break_stmt = tkn;
		return self;
	}

	Stmt*
	stmt_continue_new(mn::Allocator arena, Tkn tkn)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_CONTINUE;
		self->arena = arena;
		self->continue_stmt = tkn;
		return self;
	}

	Stmt*
	stmt_discard_new(mn::Allocator arena, Tkn tkn)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_DISCARD;
		self->arena = arena;
		self->discard_stmt = tkn;
		return self;
	}

	Stmt*
	stmt_return_new(mn::Allocator arena, Expr* expr)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_RETURN;
		self->arena = arena;
		self->return_stmt = expr;
		return self;
	}

	Stmt*
	stmt_block_new(mn::Allocator arena, mn::Buf<Stmt*> stmts)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_BLOCK;
		self->arena = arena;
		self->block_stmt = stmts;
		return self;
	}

	Stmt*
	stmt_if_new(mn::Allocator arena, mn::Buf<Expr*> cond, mn::Buf<Stmt*> body, Stmt* else_body)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_IF;
		self->arena = arena;
		self->if_stmt.cond = cond;
		self->if_stmt.body = body;
		self->if_stmt.else_body = else_body;
		return self;
	}

	Stmt*
	stmt_for_new(mn::Allocator arena, Stmt* init, Expr* cond, Stmt* post, Stmt* body)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_FOR;
		self->arena = arena;
		self->for_stmt.init = init;
		self->for_stmt.cond = cond;
		self->for_stmt.post = post;
		self->for_stmt.body = body;
		return self;
	}

	Stmt*
	stmt_assign_new(mn::Allocator arena, mn::Buf<Expr*> lhs, Tkn op, mn::Buf<Expr*> rhs)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_ASSIGN;
		self->arena = arena;
		self->assign_stmt.lhs = lhs;
		self->assign_stmt.op = op;
		self->assign_stmt.rhs = rhs;
		return self;
	}

	Stmt*
	stmt_expr_new(mn::Allocator arena, Expr* expr)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_EXPR;
		self->arena = arena;
		self->expr_stmt = expr;
		return self;
	}

	Stmt*
	stmt_decl_new(mn::Allocator arena, Decl* decl)
	{
		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = Stmt::KIND_DECL;
		self->arena = arena;
		self->decl_stmt = decl;
		return self;
	}

	Stmt*
	stmt_clone(const Stmt* other, mn::Allocator arena)
	{
		if (other == nullptr)
			return nullptr;

		auto self = mn::alloc_zerod_from<Stmt>(arena);
		self->kind = other->kind;
		self->arena = arena;
		self->loc = other->loc;
		switch (other->kind)
		{
		case Stmt::KIND_BREAK:
			self->break_stmt = other->break_stmt;
			break;
		case Stmt::KIND_CONTINUE:
			self->continue_stmt = other->continue_stmt;
			break;
		case Stmt::KIND_DISCARD:
			self->discard_stmt = other->discard_stmt;
			break;
		case Stmt::KIND_RETURN:
			self->return_stmt = clone(other->return_stmt);
			break;
		case Stmt::KIND_IF:
			self->if_stmt.cond = mn::buf_clone(other->if_stmt.cond, arena);
			self->if_stmt.body = mn::buf_clone(other->if_stmt.body, arena);
			self->if_stmt.else_body = clone(other->if_stmt.else_body);
			break;
		case Stmt::KIND_FOR:
			self->for_stmt.init = clone(other->for_stmt.init);
			self->for_stmt.cond = clone(other->for_stmt.cond);
			self->for_stmt.post = clone(other->for_stmt.post);
			self->for_stmt.body = clone(other->for_stmt.body);
			break;
		case Stmt::KIND_ASSIGN:
			self->assign_stmt.lhs = mn::buf_clone(other->assign_stmt.lhs, arena);
			self->assign_stmt.rhs = mn::buf_clone(other->assign_stmt.rhs, arena);
			self->assign_stmt.op = other->assign_stmt.op;
			break;
		case Stmt::KIND_EXPR:
			self->expr_stmt = clone(other->expr_stmt);
			break;
		case Stmt::KIND_BLOCK:
			self->block_stmt = mn::buf_clone(other->block_stmt, arena);
			break;
		case Stmt::KIND_DECL:
			self->decl_stmt = clone(other->decl_stmt);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
		return self;
	}

	Tag
	tag_new(mn::Allocator arena)
	{
		Tag self{};
		self.args = mn::map_with_allocator<const char*, Tag_Key_Value>(arena);
		return self;
	}

	Tag_Table
	tag_table_new(mn::Allocator arena)
	{
		Tag_Table self{};
		self.table = mn::map_with_allocator<const char*, Tag>(arena);
		return self;
	}

	Arg
	arg_new(mn::Allocator arena)
	{
		Arg self{};
		self.tags = tag_table_new(arena);
		self.names = mn::buf_with_allocator<Tkn>(arena);
		self.type = type_sign_new(arena);
		return self;
	}

	Arg
	arg_clone(const Arg& other, mn::Allocator arena)
	{
		auto self = other;
		self.tags = other.tags;
		self.names = mn::buf_memcpy_clone(other.names, arena);
		self.type = type_sign_clone(other.type, arena);
		return self;
	}

	Field
	field_new(mn::Allocator arena)
	{
		Field self{};
		self.tags = tag_table_new(arena);
		self.names = mn::buf_with_allocator<Tkn>(arena);
		self.type = type_sign_new(arena);
		return self;
	}

	Field
	field_clone(const Field& other, mn::Allocator arena)
	{
		Field self{};
		self.tags = other.tags;
		self.names = mn::buf_memcpy_clone(other.names, arena);
		self.type = type_sign_clone(other.type, arena);
		self.default_value = clone(other.default_value);
		return self;
	}

	Enum_Field
	enum_field_clone(const Enum_Field& other)
	{
		Enum_Field self{};
		self.name = other.name;
		self.value = clone(other.value);
		return self;
	}

	Template_Arg
	template_arg_clone(const Template_Arg& other, mn::Allocator arena)
	{
		return {mn::buf_memcpy_clone(other.names, arena)};
	}

	Decl*
	decl_var_new(mn::Allocator arena, mn::Buf<Tkn> names, mn::Buf<Expr*> values, Type_Sign type)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_VAR;
		self->arena = arena;
		self->var_decl.names = names;
		self->var_decl.values = values;
		self->var_decl.type = type;
		return self;
	}

	Decl*
	decl_convert_var_to_const(Decl* var)
	{
		auto self = var;
		self->kind = Decl::KIND_CONST;
		self->const_decl.names = var->var_decl.names;
		self->const_decl.values = var->var_decl.values;
		self->const_decl.type = var->var_decl.type;
		return self;
	}

	Decl*
	decl_func_new(mn::Allocator arena, Tkn name, mn::Buf<Arg> args, Type_Sign ret, Stmt* body, mn::Buf<Template_Arg> template_args)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_FUNC;
		self->arena = arena;
		self->name = name;
		self->template_args = template_args;
		self->func_decl.args = args;
		self->func_decl.return_type = ret;
		self->func_decl.body = body;
		return self;
	}

	Decl*
	decl_struct_new(mn::Allocator arena, Tkn name, mn::Buf<Field> fields, mn::Buf<Template_Arg> template_args)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_STRUCT;
		self->arena = arena;
		self->name = name;
		self->template_args = template_args;
		self->struct_decl.fields = fields;
		return self;
	}

	Decl*
	decl_import_new(mn::Allocator arena, Tkn path, Tkn name)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_IMPORT;
		self->arena = arena;
		self->name = name;
		self->import_decl.path = path;
		self->import_decl.name = name;
		return self;
	}

	Decl*
	decl_if_new(mn::Allocator arena, mn::Buf<Expr*> cond, mn::Buf<mn::Buf<Decl*>> body, mn::Buf<Decl*> else_body)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_IF;
		self->arena = arena;
		self->if_decl.cond = cond;
		self->if_decl.body = body;
		self->if_decl.else_body = else_body;
		return self;
	}

	Decl*
	decl_enum_new(mn::Allocator arena, Tkn name, mn::Buf<Enum_Field> fields)
	{
		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = Decl::KIND_ENUM;
		self->arena = arena;
		self->name = name;
		self->enum_decl.fields = fields;
		return self;
	}

	Decl*
	decl_clone(const Decl* other, mn::Allocator arena)
	{
		if (other == nullptr)
			return nullptr;

		auto self = mn::alloc_zerod_from<Decl>(arena);
		self->kind = other->kind;
		self->arena = arena;
		self->loc = other->loc;
		self->name = other->name;
		self->tags = other->tags;
		self->template_args = mn::buf_clone(other->template_args, arena);

		switch (other->kind)
		{
		case Decl::KIND_CONST:
			self->const_decl.names = mn::buf_memcpy_clone(other->const_decl.names, arena);
			self->const_decl.values = mn::buf_clone(other->const_decl.values, arena);
			self->const_decl.type = clone(other->const_decl.type);
			break;
		case Decl::KIND_VAR:
			self->var_decl.names = mn::buf_memcpy_clone(other->var_decl.names, arena);
			self->var_decl.values = mn::buf_clone(other->var_decl.values, arena);
			self->var_decl.type = clone(other->var_decl.type);
			break;
		case Decl::KIND_FUNC:
			self->func_decl.args = mn::buf_clone(other->func_decl.args, arena);
			self->func_decl.return_type = clone(other->func_decl.return_type);
			self->func_decl.body = clone(other->func_decl.body);
			self->func_decl.is_geometry = other->func_decl.is_geometry;
			self->func_decl.geometry_output = other->func_decl.geometry_output;
			self->func_decl.geometry_max_vertex_count = other->func_decl.geometry_max_vertex_count;
			self->func_decl.geometry_in = other->func_decl.geometry_in;
			self->func_decl.geometry_out = other->func_decl.geometry_out;
			self->func_decl.has_emits = other->func_decl.has_emits;
			break;
		case Decl::KIND_STRUCT:
			self->struct_decl.fields = mn::buf_clone(other->struct_decl.fields, arena);
			break;
		case Decl::KIND_IMPORT:
			self->import_decl.path = other->import_decl.path;
			self->import_decl.name = other->import_decl.name;
			break;
		case Decl::KIND_IF:
			self->if_decl.cond = mn::buf_clone(other->if_decl.cond, arena);
			self->if_decl.body = mn::buf_clone(other->if_decl.body, arena);
			self->if_decl.else_body = mn::buf_clone(other->if_decl.else_body, arena);
			break;
		case Decl::KIND_ENUM:
			self->enum_decl.fields = mn::buf_clone(other->enum_decl.fields, arena);
			break;
		default:
			assert(false && "unreachable");
			break;
		}
		return self;
	}
}