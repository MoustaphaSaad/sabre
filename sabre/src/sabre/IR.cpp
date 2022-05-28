#include "sabre/IR.h"

#include <mn/Assert.h>

namespace sabre::spirv
{
	inline static Value*
	_module_value_new(Module* self, Type* type)
	{
		auto value = mn::alloc_zerod_from<Value>(self->arena);
		value->id = ++self->id_generator;
		value->type = type;

		mn::map_insert(self->entities, value->id, entity_from_value(value));
		return value;
	}

	// API
	Entity
	entity_from_type(Type* type)
	{
		Entity self{};
		self.kind = Entity::KIND_TYPE;
		self.id = type->id;
		self.as_type = type;
		return self;
	}

	Entity
	entity_from_func(Func* func)
	{
		Entity self{};
		self.kind = Entity::KIND_FUNC;
		self.id = func->id;
		self.as_func = func;
		return self;
	}

	Entity
	entity_from_basic_block(Basic_Block* basic_block)
	{
		Entity self{};
		self.kind = Entity::KIND_BASIC_BLOCK;
		self.id = basic_block->id;
		self.as_basic_block = basic_block;
		return self;
	}

	Entity
	entity_from_value(Value* value)
	{
		Entity self{};
		self.kind = Entity::KIND_VALUE;
		self.id = value->id;
		self.as_value = value;
		return self;
	}

	Entity
	entity_from_constant(Value* value, int data)
	{
		Entity self{};
		self.kind = Entity::KIND_CONSTANT;
		self.id = value->id;
		self.as_constant.value = value;
		self.as_constant.data.as_int = data;
		return self;
	}

	Entity
	entity_from_constant(Value* value, bool data)
	{
		Entity self{};
		self.kind = Entity::KIND_CONSTANT;
		self.id = value->id;
		self.as_constant.value = value;
		self.as_constant.data.as_bool = data;
		return self;
	}

	Value*
	basic_block_add(Basic_Block* self, Value* op1, Value* op2)
	{
		mn_assert(self->terminated == false);

		if (type_is_int(op1->type) && type_is_int(op2->type))
		{
			Instruction ins{};
			ins.kind = Instruction::Op_IAdd;
			ins.as_iadd.op1 = op1;
			ins.as_iadd.op2 = op2;
			ins.as_iadd.res = _module_value_new(self->func->module, op1->type);
			mn::buf_push(self->instructions, ins);

			return ins.as_iadd.res;
		}
		else
		{
			mn_unreachable();
			return nullptr;
		}
	}

	Value*
	basic_block_sub(Basic_Block* self, Value* op1, Value* op2)
	{
		mn_assert(self->terminated == false);

		if (type_is_int(op1->type) && type_is_int(op2->type))
		{
			Instruction ins{};
			ins.kind = Instruction::Op_ISub;
			ins.as_isub.op1 = op1;
			ins.as_isub.op2 = op2;
			ins.as_isub.res = _module_value_new(self->func->module, op1->type);
			mn::buf_push(self->instructions, ins);

			return ins.as_isub.res;
		}
		else
		{
			mn_unreachable();
			return nullptr;
		}
	}

	Value*
	basic_block_mul(Basic_Block* self, Value* op1, Value* op2)
	{
		mn_assert(self->terminated == false);

		if (type_is_int(op1->type) && type_is_int(op2->type))
		{
			Instruction ins{};
			ins.kind = Instruction::Op_IMul;
			ins.as_imul.op1 = op1;
			ins.as_imul.op2 = op2;
			ins.as_imul.res = _module_value_new(self->func->module, op1->type);
			mn::buf_push(self->instructions, ins);

			return ins.as_imul.res;
		}
		else
		{
			mn_unreachable();
			return nullptr;
		}
	}

	Value*
	basic_block_div(Basic_Block* self, Value* op1, Value* op2)
	{
		mn_assert(self->terminated == false);

		if (type_is_int(op1->type) && type_is_int(op2->type))
		{
			Instruction ins{};
			ins.kind = Instruction::Op_SDiv;
			ins.as_imul.op1 = op1;
			ins.as_imul.op2 = op2;
			ins.as_imul.res = _module_value_new(self->func->module, op1->type);
			mn::buf_push(self->instructions, ins);

			return ins.as_imul.res;
		}
		else
		{
			mn_unreachable();
			return nullptr;
		}
	}

	Value*
	basic_block_bitwise_and(Basic_Block* self, Value* op1, Value* op2)
	{
		mn_assert(self->terminated == false);

		if (type_is_int(op1->type) && type_is_int(op2->type))
		{
			Instruction ins{};
			ins.kind = Instruction::Op_BitwiseAnd;
			ins.as_bitwise_and.op1 = op1;
			ins.as_bitwise_and.op2 = op2;
			ins.as_bitwise_and.res = _module_value_new(self->func->module, op1->type);
			mn::buf_push(self->instructions, ins);

			return ins.as_bitwise_and.res;
		}
		else
		{
			mn_unreachable();
			return nullptr;
		}
	}

	Value*
	basic_block_equal(Basic_Block* self, Value* op1, Value* op2)
	{
		mn_assert(self->terminated == false);

		if (type_is_int(op1->type) && type_is_int(op2->type))
		{
			Instruction ins{};
			ins.kind = Instruction::Op_IEqual;
			ins.as_iequal.op1 = op1;
			ins.as_iequal.op2 = op2;
			ins.as_iequal.res = _module_value_new(self->func->module, module_type_bool_new(self->func->module));
			mn::buf_push(self->instructions, ins);

			return ins.as_iequal.res;
		}
		else
		{
			mn_unreachable();
			return nullptr;
		}
	}

	Value*
	basic_block_ret(Basic_Block* self, Value* res)
	{
		mn_assert(self->terminated == false);

		Instruction ins{};
		ins.kind = Instruction::Op_ReturnValue;
		ins.as_return.value = res;
		mn::buf_push(self->instructions, ins);

		self->terminated = true;

		return ins.as_return.value;
	}

	Value*
	basic_block_variable(Basic_Block* self, Type* type, STORAGE_CLASS storage_class, Value* init)
	{
		mn_assert(self->terminated == false);

		Instruction ins{};
		ins.kind = Instruction::Op_Variable;
		ins.as_variable.type = type;
		ins.as_variable.storage_class = storage_class;
		ins.as_variable.init = init;
		ins.as_variable.res = _module_value_new(self->func->module, type);
		mn::buf_push(self->instructions, ins);

		return ins.as_variable.res;
	}

	Value*
	basic_block_load(Basic_Block* self, Type* type, Value* src)
	{
		mn_assert(self->terminated == false);

		Instruction ins{};
		ins.kind = Instruction::Op_Load;
		ins.as_load.type = type;
		ins.as_load.src = src;
		ins.as_load.res = _module_value_new(self->func->module, type);
		mn::buf_push(self->instructions, ins);

		return ins.as_load.res;
	}

	void
	basic_block_store(Basic_Block* self, Value* src, Value* dst)
	{
		mn_assert(self->terminated == false);

		Instruction ins{};
		ins.kind = Instruction::Op_Store;
		ins.as_store.src = src;
		ins.as_store.dst = dst;
		mn::buf_push(self->instructions, ins);
	}

	void
	basic_block_branch_conditional(Basic_Block* self, Value* cond, Basic_Block* true_branch, Basic_Block* false_branch, Basic_Block* merge_branch)
	{
		mn_assert(self->terminated == false);

		Instruction ins{};
		ins.kind = Instruction::Op_SelectionMerge;
		ins.as_selection_merge.merge_branch = merge_branch;
		mn::buf_push(self->instructions, ins);

		ins.kind = Instruction::Op_BranchConditional;
		ins.as_branch_conditional.cond = cond;
		ins.as_branch_conditional.true_branch = true_branch;
		ins.as_branch_conditional.false_branch = false_branch;
		ins.as_branch_conditional.merge_branch = merge_branch;
		mn::buf_push(self->instructions, ins);

		self->terminated = true;
	}

	void
	basic_block_branch(Basic_Block* self, Basic_Block* branch)
	{
		mn_assert(self->terminated == false);

		Instruction ins{};
		ins.kind = Instruction::Op_Branch;
		ins.as_branch.branch = branch;
		mn::buf_push(self->instructions, ins);

		self->terminated = true;
	}

	void
	basic_block_unreachable(Basic_Block* self)
	{
		mn_assert(self->terminated == false);
		mn_assert(self->instructions.count == 0);

		Instruction ins{};
		ins.kind = Instruction::Op_Unreachable;
		mn::buf_push(self->instructions, ins);

		self->terminated = true;
	}

	Module*
	module_new()
	{
		auto self = mn::alloc_zerod<Module>();
		self->arena = mn::allocator_arena_new();
		return self;
	}

	void
	module_free(Module* self)
	{
		if (self)
		{
			mn::allocator_free(self->arena);
			mn::map_free(self->entities);
			mn::map_free(self->type_cache);
			mn::map_free(self->constant_cache);
			mn::free(self);
		}
	}

	Type*
	module_type_void_new(Module* self)
	{
		Type key{};
		key.kind = Type::KIND_VOID;
		if (auto it = mn::map_lookup(self->type_cache, key))
			return it->value;

		auto type = mn::alloc_zerod_from<Type>(self->arena);
		type->kind = Type::KIND_VOID;
		type->id = ++self->id_generator;

		mn::map_insert(self->entities, type->id, entity_from_type(type));
		mn::map_insert(self->type_cache, key, type);
		return type;
	}

	Type*
	module_type_bool_new(Module* self)
	{
		Type key{};
		key.kind = Type::KIND_BOOL;
		if (auto it = mn::map_lookup(self->type_cache, key))
			return it->value;

		auto type = mn::alloc_zerod_from<Type>(self->arena);
		type->kind = Type::KIND_BOOL;
		type->id = ++self->id_generator;

		mn::map_insert(self->entities, type->id, entity_from_type(type));
		mn::map_insert(self->type_cache, key, type);
		return type;
	}

	Type*
	module_type_int_new(Module* self, int bit_width, bool is_signed)
	{
		Type key{};
		key.kind = Type::KIND_INT;
		key.as_int.bit_width = bit_width;
		key.as_int.is_signed = is_signed;
		if (auto it = mn::map_lookup(self->type_cache, key))
			return it->value;

		auto type = mn::alloc_zerod_from<Type>(self->arena);
		type->kind = Type::KIND_INT;
		type->id = ++self->id_generator;
		type->as_int.bit_width = bit_width;
		type->as_int.is_signed = is_signed ? 1 : 0;

		mn::map_insert(self->entities, type->id, entity_from_type(type));
		mn::map_insert(self->type_cache, key, type);
		return type;
	}

	Type*
	module_type_pointer_new(Module* self, Type* base, STORAGE_CLASS storage_class)
	{
		Type key{};
		key.kind = Type::KIND_PTR;
		key.as_ptr.base = base;
		key.as_ptr.storage_class = storage_class;
		if (auto it = mn::map_lookup(self->type_cache, key))
			return it->value;

		auto type = mn::alloc_zerod_from<Type>(self->arena);
		type->kind = Type::KIND_PTR;
		type->id = ++self->id_generator;
		type->as_ptr.base = base;
		type->as_ptr.storage_class = storage_class;

		mn::map_insert(self->entities, type->id, entity_from_type(type));
		mn::map_insert(self->type_cache, key, type);
		return type;
	}

	Type*
	module_type_func_new(Module* self, Type* return_type)
	{
		auto type = mn::alloc_zerod_from<Type>(self->arena);
		type->kind = Type::KIND_FUNC;
		type->id = ++self->id_generator;
		type->as_func.return_type = return_type;
		type->as_func.args = mn::buf_with_allocator<Type*>(self->arena);

		mn::map_insert(self->entities, type->id, entity_from_type(type));
		return type;
	}

	void
	module_type_func_arg(Type* func, Type* arg)
	{
		mn_assert(func->kind == Type::KIND_FUNC);

		mn::buf_push(func->as_func.args, arg);
	}

	Value*
	module_int_constant(Module* self, Type* type, int data)
	{
		Constant_Key key{};
		key.type = type;
		key.value.as_int = data;
		if (auto it = mn::map_lookup(self->constant_cache, key))
			return it->value;

		auto value = _module_value_new(self, type);
		mn::map_insert(self->entities, value->id, entity_from_constant(value, data));

		mn::map_insert(self->constant_cache, key, value);
		return value;
	}

	Value*
	module_bool_constant(Module* self, bool data)
	{
		Constant_Key key{};
		key.type = module_type_bool_new(self);
		key.value.as_bool = data;
		if (auto it = mn::map_lookup(self->constant_cache, key))
			return it->value;

		auto bool_type = module_type_bool_new(self);
		auto value = _module_value_new(self, bool_type);
		mn::map_insert(self->entities, value->id, entity_from_constant(value, data));

		mn::map_insert(self->constant_cache, key, value);
		return value;
	}

	Func*
	module_func_new(Module* self, Type* func_type)
	{
		mn_assert(func_type->kind == Type::KIND_FUNC);

		auto func = mn::alloc_zerod_from<Func>(self->arena);
		func->module = self;
		func->id = ++self->id_generator;
		func->type = func_type;
		func->args = mn::buf_with_allocator<Value*>(self->arena);
		func->blocks = mn::buf_with_allocator<Basic_Block*>(self->arena);

		mn::buf_reserve(func->args, func_type->as_func.args.count);
		for (auto arg_type: func_type->as_func.args)
			mn::buf_push(func->args, _module_value_new(self, arg_type));

		mn::map_insert(self->entities, func->id, entity_from_func(func));
		return func;
	}

	Basic_Block*
	func_basic_block_new(Func* self)
	{
		auto module = self->module;

		auto basic_block = mn::alloc_zerod_from<Basic_Block>(module->arena);
		basic_block->func = self;
		basic_block->id = ++module->id_generator;
		basic_block->instructions = mn::buf_with_allocator<Instruction>(self->module->arena);

		mn::map_insert(module->entities, basic_block->id, entity_from_basic_block(basic_block));
		mn::buf_push(self->blocks, basic_block);

		return basic_block;
	}

	Value*
	func_arg(Func* self, size_t i)
	{
		return self->args[i];
	}
}