#include "sabre/SPIRV.h"
#include "sabre/IR.h"
#include "sabre/Unit.h"
#include "sabre/Scope.h"
#include "sabre/Type_Interner.h"

#include <mn/Defer.h>
#include <mn/Assert.h>

namespace sabre
{
	inline static void
	_spirv_enter_func(SPIRV& self, spirv::Func* f)
	{
		mn::buf_push(self.func_stack, f);
	}

	inline static void
	_spirv_leave_func(SPIRV& self)
	{
		mn::buf_pop(self.func_stack);
	}

	inline static spirv::Func*
	_spirv_current_func(SPIRV& self)
	{
		if (self.func_stack.count > 0)
			return mn::buf_top(self.func_stack);
		return nullptr;
	}

	inline static void
	_spirv_enter_bb(SPIRV& self, spirv::Basic_Block* bb)
	{
		mn::buf_push(self.bb_stack, bb);
	}

	inline static void
	_spirv_leave_bb(SPIRV& self)
	{
		mn::buf_pop(self.bb_stack);
	}

	inline static spirv::Basic_Block*
	_spirv_current_bb(SPIRV& self)
	{
		if (self.bb_stack.count > 0)
			return mn::buf_top(self.bb_stack);
		return nullptr;
	}

	inline static void
	_spirv_enter_scope(SPIRV& self, Scope* scope)
	{
		mn::buf_push(self.scope_stack, scope);
	}

	inline static void
	_spirv_leave_scope(SPIRV& self)
	{
		mn::buf_pop(self.scope_stack);
	}

	inline static Scope*
	_spirv_current_scope(SPIRV& self)
	{
		return mn::buf_top(self.scope_stack);
	}

	inline static Value_Table*
	_spirv_value_table_of_scope(SPIRV& self, Scope* scope)
	{
		if (auto it = mn::map_lookup(self.scope_value_table, scope))
			return it->value;

		Value_Table* res = nullptr;
		if (scope->parent == nullptr)
			res = value_table_new(nullptr);
		else
			res = value_table_new(_spirv_value_table_of_scope(self, scope->parent));
		mn::map_insert(self.scope_value_table, scope, res);
		return res;
	}

	inline static Value_Table*
	_spirv_current_value_table(SPIRV& self)
	{
		return _spirv_value_table_of_scope(self, _spirv_current_scope(self));
	}

	inline static spirv::Type*
	_spirv_ptr_gen(SPIRV& self, spirv::Type* type, spirv::STORAGE_CLASS storage_class)
	{
		if (auto it = mn::map_lookup(self.spirv_type_pointer_table, type))
			return it->value;
		auto res = spirv::module_type_pointer_new(self.out, type, storage_class);
		mn::map_insert(self.spirv_type_pointer_table, type, res);
		return res;
	}

	inline static spirv::Type*
	_spirv_type_gen(SPIRV& self, Type* type)
	{
		if (auto it = mn::map_lookup(self.spirv_type_table, type))
			return it->value;

		spirv::Type* res = nullptr;

		switch (type->kind)
		{
		case Type::KIND_VOID:
			res = spirv::module_type_void_new(self.out);
			break;
		case Type::KIND_BOOL:
			res = spirv::module_type_bool_new(self.out);
			break;
		case Type::KIND_INT:
			res = spirv::module_type_int_new(self.out, 32, true);
			break;
		case Type::KIND_FUNC:
		{
			auto return_type = _spirv_type_gen(self, type->as_func.sign.return_type);
			res = spirv::module_type_func_new(self.out, return_type);
			for (auto arg_type: type->as_func.sign.args.types)
				spirv::module_type_func_arg(res, _spirv_type_gen(self, arg_type));
			break;
		}
		default:
			mn_unreachable();
			break;
		}

		if (res)
			mn::map_insert(self.spirv_type_table, type, res);

		return res;
	}

	inline static spirv::Value*
	_spirv_expr_gen(SPIRV& self, Expr* expr);

	inline static spirv::Value*
	_spirv_binary_op_gen(SPIRV& self, spirv::Value* lhs, spirv::Value* rhs, Tkn::KIND op)
	{
		auto bb = _spirv_current_bb(self);
		switch (op)
		{
		case Tkn::KIND_PLUS:
			return spirv::basic_block_add(bb, lhs, rhs);
		case Tkn::KIND_MINUS:
			return spirv::basic_block_sub(bb, lhs, rhs);
		case Tkn::KIND_STAR:
			return spirv::basic_block_mul(bb, lhs, rhs);
		case Tkn::KIND_DIVIDE:
			return spirv::basic_block_div(bb, lhs, rhs);
		case Tkn::KIND_BIT_AND:
			return spirv::basic_block_bitwise_and(bb, lhs, rhs);
		case Tkn::KIND_EQUAL_EQUAL:
			return spirv::basic_block_equal(bb, lhs, rhs);
		default:
			break;
		}
		return nullptr;
	}

	inline static spirv::Value*
	_spirv_expr_atom_gen(SPIRV& self, Expr* expr)
	{
		switch (expr->atom.tkn.kind)
		{
		case Tkn::KIND_ID:
		{
			auto vt = _spirv_current_value_table(self);
			return value_table_find(vt, expr->atom.tkn.str);
		}
		case Tkn::KIND_LITERAL_INTEGER:
		{
			auto type = _spirv_type_gen(self, expr->type);
			return spirv::module_int_constant(self.out, type, expr->const_value.as_int);
		}
		case Tkn::KIND_KEYWORD_TRUE:
		{
			return spirv::module_bool_constant(self.out, true);
		}
		case Tkn::KIND_KEYWORD_FALSE:
		{
			return spirv::module_bool_constant(self.out, false);
		}
		default:
			mn_unreachable();
			return nullptr;
		}
	}

	inline static spirv::Value*
	_spirv_expr_binary_gen(SPIRV& self, Expr* expr)
	{
		auto lhs = _spirv_expr_gen(self, expr->binary.left);
		auto rhs = _spirv_expr_gen(self, expr->binary.right);

		return _spirv_binary_op_gen(self, lhs, rhs, expr->binary.op.kind);
	}

	inline static spirv::Value*
	_spirv_expr_gen(SPIRV& self, Expr* expr)
	{
		switch (expr->kind)
		{
		case Expr::KIND_ATOM:
			return _spirv_expr_atom_gen(self, expr);
		case Expr::KIND_BINARY:
			return _spirv_expr_binary_gen(self, expr);
		default:
			return nullptr;
		}
	}

	inline static void
	_spirv_stmt_gen(SPIRV& self, Stmt* stmt);

	inline static void
	_spirv_decl_gen(SPIRV& self, Decl* decl);

	inline static void
	_spirv_symbol_gen(SPIRV& self, Symbol* sym);

	inline static void
	_spirv_stmt_block_gen(SPIRV& self, Stmt* stmt)
	{
		for (auto s: stmt->block_stmt)
			_spirv_stmt_gen(self, s);
	}

	inline static spirv::Value*
	_spirv_stmt_expr_gen(SPIRV& self, Stmt* stmt)
	{
		return _spirv_expr_gen(self, stmt->expr_stmt);
	}

	inline static void
	_spirv_stmt_if_gen(SPIRV& self, Stmt* stmt)
	{
		auto func = _spirv_current_func(self);

		spirv::Basic_Block* false_bb = nullptr;
		auto merge_bbs = mn::buf_with_allocator<spirv::Basic_Block*>(mn::memory::tmp());
		for (size_t i = 0; i < stmt->if_stmt.cond.count; ++i)
		{
			auto cond = _spirv_expr_gen(self, stmt->if_stmt.cond[0]);

			auto true_bb = spirv::func_basic_block_new(func);
			spirv::Basic_Block* merge_bb = nullptr;

			if (i + 1 < stmt->if_stmt.cond.count)
			{
				false_bb = spirv::func_basic_block_new(func);
				merge_bb = spirv::func_basic_block_new(func);
				mn::buf_push(merge_bbs, merge_bb);
			}
			else
			{
				// if we have else branch we'll generate two blocks as usual
				if (stmt->if_stmt.else_body)
				{
					false_bb = spirv::func_basic_block_new(func);
					merge_bb = spirv::func_basic_block_new(func);
					mn::buf_push(merge_bbs, merge_bb);
				}
				// if we don't have an else, the false will be the merge branch
				else
				{
					false_bb = spirv::func_basic_block_new(func);
					merge_bb = false_bb;
				}
			}

			auto current_bb = _spirv_current_bb(self);
			spirv::basic_block_branch_conditional(current_bb, cond, true_bb, false_bb, merge_bb);

			_spirv_enter_bb(self, true_bb);
			{
				_spirv_stmt_gen(self, stmt->if_stmt.body[0]);
			}
			_spirv_leave_bb(self);

			// continue with the false branch
			_spirv_enter_bb(self, false_bb);
		}

		if (stmt->if_stmt.else_body)
			_spirv_stmt_gen(self, stmt->if_stmt.else_body);

		for (size_t i = 1; i < merge_bbs.count; ++i)
		{
			auto top_bb = merge_bbs[merge_bbs.count - i - 1 + 1];
			auto prev_bb = merge_bbs[merge_bbs.count - i - 1];

			_spirv_enter_bb(self, top_bb);
			{
				spirv::basic_block_branch(top_bb, prev_bb);
			}
			_spirv_leave_bb(self);
		}

		if (merge_bbs.count > 0)
			_spirv_enter_bb(self, merge_bbs[0]);
	}

	inline static void
	_spirv_stmt_return_gen(SPIRV& self, Stmt* stmt)
	{
		auto bb = _spirv_current_bb(self);
		auto res = _spirv_expr_gen(self, stmt->return_stmt);
		spirv::basic_block_ret(bb, res);
	}

	inline static void
	_spirv_stmt_decl_gen(SPIRV& self, Stmt* stmt)
	{
		_spirv_decl_gen(self, stmt->decl_stmt);
	}

	inline static void
	_spirv_stmt_gen(SPIRV& self, Stmt* stmt)
	{
		switch (stmt->kind)
		{
		case Stmt::KIND_BLOCK:
			_spirv_stmt_block_gen(self, stmt);
			break;
		case Stmt::KIND_RETURN:
			_spirv_stmt_return_gen(self, stmt);
			break;
		case Stmt::KIND_EXPR:
			_spirv_stmt_expr_gen(self, stmt);
			break;
		case Stmt::KIND_IF:
			_spirv_stmt_if_gen(self, stmt);
			break;
		case Stmt::KIND_DECL:
			_spirv_stmt_decl_gen(self, stmt);
			break;
		default:
			break;
		}
	}

	inline static void
	_spirv_decl_var_gen(SPIRV& self, Decl* decl)
	{
		auto current_scope = _spirv_current_scope(self);
		for (auto v: decl->var_decl.names)
		{
			auto sym = scope_find(current_scope, v.str);
			_spirv_symbol_gen(self, sym);
		}
	}

	inline static void
	_spirv_decl_gen(SPIRV& self, Decl* decl)
	{
		switch (decl->kind)
		{
		case Decl::KIND_VAR:
			_spirv_decl_var_gen(self, decl);
			break;
		}
	}

	inline static void
	_spirv_func_gen(SPIRV& self, Symbol* sym)
	{
		auto func = spirv::module_func_new(self.out, _spirv_type_gen(self, sym->type));

		auto d = sym->func_sym.decl;

		// enter function scope
		if (d->func_decl.body != nullptr)
			_spirv_enter_scope(self, unit_scope_find(self.unit->parent_unit, d));
		mn_defer{if (d->func_decl.body) _spirv_leave_scope(self);};

		// add function arguments to value table
		auto vt = _spirv_current_value_table(self);
		size_t arg_index = 0;
		for (auto arg: d->func_decl.args)
		{
			for (auto name: arg.names)
			{
				auto v = spirv::func_arg(func, arg_index);
				value_table_add(vt, name.str, v);
				++arg_index;
			}
		}

		// enter spirv func
		_spirv_enter_func(self, func);
		mn_defer{_spirv_leave_func(self);};

		if (d->func_decl.body)
		{
			func->entry = spirv::func_basic_block_new(func);
			_spirv_enter_bb(self, func->entry);
			mn_defer{_spirv_leave_bb(self);};

			_spirv_stmt_gen(self, d->func_decl.body);
		}
	}

	inline static void
	_spirv_var_gen(SPIRV& self, Symbol* sym)
	{
		auto vt = _spirv_current_value_table(self);
		auto type = _spirv_type_gen(self, sym->type);
		auto ptr_type = _spirv_ptr_gen(self, type, spirv::STORAGE_CLASS_FUNCTION);
		auto bb = _spirv_current_bb(self);

		spirv::Value* initial_value = nullptr;

		auto value = sym->var_sym.value;
		// constants can be emitted and assigned in the initial value, we have it set to nullptr for now
		// later we can emit the constant and reference it here, it also works for globals
		if (value && value->mode == ADDRESS_MODE_CONST)
		{
			initial_value = _spirv_expr_gen(self, value);
		}

		auto res = spirv::basic_block_variable(bb, ptr_type, spirv::STORAGE_CLASS_FUNCTION, initial_value);
		value_table_add(vt, sym->name, res);

		// emit load and store instruction here
		if (value && sym->var_sym.value->mode != ADDRESS_MODE_CONST)
		{
			auto init_value = _spirv_expr_gen(self, value);
			auto loaded_value = spirv::basic_block_load(bb, type, init_value);
			spirv::basic_block_store(bb, loaded_value, res);
		}
	}

	inline static void
	_spirv_symbol_gen(SPIRV& self, Symbol* sym)
	{
		switch(sym->kind)
		{
		case Symbol::KIND_FUNC:
			_spirv_func_gen(self, sym);
			break;
		case Symbol::KIND_VAR:
			_spirv_var_gen(self, sym);
			break;
		default:
			mn_unreachable();
			break;
		}
	}

	// API
	Value_Table*
	value_table_new(Value_Table* parent)
	{
		auto self = mn::alloc_zerod<Value_Table>();
		self->parent = parent;
		return self;
	}

	void
	value_table_free(Value_Table* self)
	{
		if (self)
		{
			mn::map_free(self->table);
			mn::free(self);
		}
	}

	spirv::Value*
	value_table_shallow_find(const Value_Table* self, const char* name)
	{
		if (auto it = mn::map_lookup(self->table, name))
			return it->value;
		return nullptr;
	}

	spirv::Value*
	value_table_find(const Value_Table* self, const char* name)
	{
		for (auto it = self; it != nullptr; it = it->parent)
		{
			if (auto v = value_table_shallow_find(self, name))
				return v;
		}
		return nullptr;
	}

	void
	value_table_add(Value_Table* self, const char* name, spirv::Value* value)
	{
		mn_assert(value_table_shallow_find(self, name) == nullptr);
		mn::map_insert(self->table, name, value);
	}

	SPIRV
	spirv_new(Unit_Package* unit)
	{
		SPIRV self{};
		self.unit = unit;
		self.out = spirv::module_new();

		// push global scope as first entry in scope stack
		auto global_scope = self.unit->global_scope;
		mn_assert(global_scope != nullptr);
		mn::buf_push(self.scope_stack, global_scope);

		return self;
	}

	void
	spirv_free(SPIRV& self)
	{
		spirv::module_free(self.out);
		mn::map_free(self.spirv_type_table);
		mn::map_free(self.spirv_type_pointer_table);

		for (auto& [_, vt]: self.scope_value_table)
			value_table_free(vt);
		mn::map_free(self.scope_value_table);

		mn::buf_free(self.func_stack);
		mn::buf_free(self.bb_stack);
		mn::buf_free(self.scope_stack);
	}

	void
	spirv_gen(SPIRV& self)
	{
		// loop over all reachable symbols and generate them
		for (auto sym: self.unit->reachable_symbols)
		{
			_spirv_symbol_gen(self, sym);
		}

		for (auto& [_, e]: self.out->entities)
		{
			if (e.kind == spirv::Entity::KIND_BASIC_BLOCK &&
				e.as_basic_block->instructions.count == 0)
			{
				spirv::basic_block_unreachable(e.as_basic_block);
			}
		}
	}
}