#pragma once

#include <mn/Map.h>
#include <mn/Memory.h>
#include <mn/Socket.h>

namespace sabre
{
	struct Type;
	struct Function;
	struct Basic_Block;
	struct Value;

	struct Module
	{
		mn::Allocator arena;
		mn::Map<mn::Str, Function*> functions_by_name;
		size_t id_generator;
	};

	struct Function
	{
		Module* parent;
		Type* return_type;
		mn::Buf<Value*> args;
		mn::Buf<Basic_Block*> basic_blocks;
	};

	struct Instruction
	{
		enum Op
		{
			Op_IAdd,
			Op_ReturnValue,
		};

		Op op;
		union
		{
			struct
			{
				Value* op1;
				Value* op2;
				Value* res;
			} as_iadd;

			struct
			{
				Value* res;
			} as_return;
		};
	};

	struct Basic_Block
	{
		Function* parent;
		mn::Buf<Instruction> instructions;
	};

	struct Value
	{
		size_t id;
		Type* type;
	};

	inline static Module*
	module_new()
	{
		auto self = mn::alloc_zerod<Module>();
		self->arena = mn::allocator_arena_new();
		return self;
	}

	inline static void
	module_free(Module* self)
	{
		if (self)
		{
			mn::allocator_free(self->arena);
			mn::map_free(self->functions_by_name);
			mn::free(self);
		}
	}

	inline static Value*
	module_value_new(Module* self, Type* type)
	{
		auto value = mn::alloc_zerod_from<Value>(self->arena);
		value->id = ++self->id_generator;
		value->type = type;
		return value;
	}

	inline static Function*
	module_func_new(Module* self, Type* return_type)
	{
		auto func = mn::alloc_zerod_from<Function>(self->arena);
		func->parent = self;
		func->return_type = return_type;
		func->args = mn::buf_with_allocator<Value*>(self->arena);
		func->basic_blocks = mn::buf_with_allocator<Basic_Block*>(self->arena);
		return func;
	}

	inline static Value*
	func_arg_new(Function* self, Type* type)
	{
		auto value = module_value_new(self->parent, type);

		mn::buf_push(self->args, value);
		return value;
	}

	inline static Basic_Block*
	func_basic_block_new(Function* self)
	{
		auto bb = mn::alloc_zerod_from<Basic_Block>(self->parent->arena);
		bb->parent = self;
		bb->instructions = mn::buf_with_allocator<Instruction>(self->parent->arena);

		mn::buf_push(self->basic_blocks, bb);
		return bb;
	}

	inline static Value*
	basic_block_iadd(Basic_Block* self, Value* a, Value* b)
	{
		Instruction ins{};
		ins.op = Instruction::Op_IAdd;
		ins.as_iadd.op1 = a;
		ins.as_iadd.op2 = b;
		ins.as_iadd.res = module_value_new(self->parent->parent, a->type);

		mn::buf_push(self->instructions, ins);
		return ins.as_iadd.res;
	}

	inline static void
	basic_block_return(Basic_Block* self, Value* res)
	{
		Instruction ins{};
		ins.op = Instruction::Op_ReturnValue;
		ins.as_return.res = res;

		mn::buf_push(self->instructions, ins);
	}
}