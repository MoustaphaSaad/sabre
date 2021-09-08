#pragma once

#include "sabre/Exports.h"

#include <mn/Map.h>
#include <mn/Memory.h>
#include <mn/Str.h>

namespace sabre::spirv
{
	struct Module;
	struct Type;
	struct Func;
	struct Basic_Block;
	struct Value;

	// represents a unique id of a SPIRV entity
	using ID = uint32_t;

	// represents a spirv entity, it can hold types, functions, etc...
	struct Entity
	{
		enum KIND
		{
			KIND_TYPE,
			KIND_FUNC,
			KIND_BASIC_BLOCK,
			KIND_VALUE,
		};

		KIND kind;
		union
		{
			Type* as_type;
			Func* as_func;
			Basic_Block* as_basic_block;
			Value* as_value;
		};
	};

	// wraps the given type in an entity
	inline static Entity
	entity_from_type(Type* type)
	{
		Entity self{};
		self.kind = Entity::KIND_TYPE;
		self.as_type = type;
		return self;
	}

	// wraps the given func in an entity
	inline static Entity
	entity_from_func(Func* func)
	{
		Entity self{};
		self.kind = Entity::KIND_FUNC;
		self.as_func = func;
		return self;
	}

	// wraps the given basic block in an entity
	inline static Entity
	entity_from_basic_block(Basic_Block* basic_block)
	{
		Entity self{};
		self.kind = Entity::KIND_BASIC_BLOCK;
		self.as_basic_block = basic_block;
		return self;
	}

	// wraps the given value in an entity
	inline static Entity
	entity_from_value(Value* value)
	{
		Entity self{};
		self.kind = Entity::KIND_VALUE;
		self.as_value = value;
		return self;
	}

	// reperesents a SPIRV type
	struct Type
	{
		enum KIND
		{
			KIND_VOID,
			KIND_INT,
			KIND_FUNC,
		};

		KIND kind;
		ID id;
		union
		{
			struct
			{
				int bit_width;
				int is_signed; // 0: unsigned, 1: signed
			} as_int;

			struct
			{
				Type* return_type;
				mn::Buf<Type*> args;
			} as_func;
		};
	};

	struct Value
	{
		ID id;
		Type* type;
	};

	// represents a SPIRV linear sequence of instructions
	struct Basic_Block
	{
		Func* func;
		ID id;
	};

	// represents a SPIRV function
	struct Func
	{
		Module* module;
		ID id;
		Type* type;
		mn::Buf<Value*> args;
	};

	// represents a SPIRV module, which is a SPIRV compilation unit
	struct Module
	{
		mn::Allocator arena;
		uint32_t id_generator;
		mn::Map<ID, Entity> entities;
	};

	// creates a new module instance
	SABRE_EXPORT Module*
	module_new();

	// frees the given module instance
	SABRE_EXPORT void
	module_free(Module* self);

	// creates a new void type instance
	SABRE_EXPORT Type*
	module_type_void_new(Module* self);

	// creates a new int type instance
	SABRE_EXPORT Type*
	module_type_int_new(Module* self, int bit_width, bool is_signed);

	// creates a new func type instance
	SABRE_EXPORT Type*
	module_type_func_new(Module* self, Type* return_type);

	// adds argument to function type
	SABRE_EXPORT void
	module_type_func_arg(Type* func, Type* arg);

	// creates a new function instance
	SABRE_EXPORT Func*
	module_func_new(Module* self, Type* func_type);

	// creates a new basic block instance
	SABRE_EXPORT Basic_Block*
	func_basic_block_new(Func* self);

	// returns function's argument value
	SABRE_EXPORT Value*
	func_arg(Func* self, size_t i);

	// struct Type;
	// struct Function;
	// struct Basic_Block;
	// struct Value;

	// struct Module
	// {
	// 	mn::Allocator arena;
	// 	mn::Map<mn::Str, Function*> functions_by_name;
	// 	size_t id_generator;
	// };

	// struct Function
	// {
	// 	Module* parent;
	// 	Type* return_type;
	// 	mn::Str name;
	// 	mn::Buf<Value*> args;
	// 	mn::Buf<Basic_Block*> basic_blocks;
	// };

	// struct Instruction
	// {
	// 	enum Op
	// 	{
	// 		Op_IAdd,
	// 		Op_ReturnValue,
	// 	};

	// 	Op op;
	// 	union
	// 	{
	// 		struct
	// 		{
	// 			Value* op1;
	// 			Value* op2;
	// 			Value* res;
	// 		} as_iadd;

	// 		struct
	// 		{
	// 			Value* res;
	// 		} as_return;
	// 	};
	// };

	// struct Basic_Block
	// {
	// 	Function* parent;
	// 	mn::Buf<Instruction> instructions;
	// };

	// struct Value
	// {
	// 	size_t id;
	// 	Type* type;
	// };

	// inline static Module*
	// module_new()
	// {
	// 	auto self = mn::alloc_zerod<Module>();
	// 	self->arena = mn::allocator_arena_new();
	// 	return self;
	// }

	// inline static void
	// module_free(Module* self)
	// {
	// 	if (self)
	// 	{
	// 		mn::allocator_free(self->arena);
	// 		mn::map_free(self->functions_by_name);
	// 		mn::free(self);
	// 	}
	// }

	// inline static Value*
	// module_value_new(Module* self, Type* type)
	// {
	// 	auto value = mn::alloc_zerod_from<Value>(self->arena);
	// 	value->id = ++self->id_generator;
	// 	value->type = type;
	// 	return value;
	// }

	// inline static Function*
	// module_func_new(Module* self, Type* return_type)
	// {
	// 	auto func = mn::alloc_zerod_from<Function>(self->arena);
	// 	func->parent = self;
	// 	func->return_type = return_type;
	// 	func->args = mn::buf_with_allocator<Value*>(self->arena);
	// 	func->basic_blocks = mn::buf_with_allocator<Basic_Block*>(self->arena);
	// 	return func;
	// }

	// inline static Value*
	// func_arg_new(Function* self, Type* type)
	// {
	// 	auto value = module_value_new(self->parent, type);

	// 	mn::buf_push(self->args, value);
	// 	return value;
	// }

	// inline static Basic_Block*
	// func_basic_block_new(Function* self)
	// {
	// 	auto bb = mn::alloc_zerod_from<Basic_Block>(self->parent->arena);
	// 	bb->parent = self;
	// 	bb->instructions = mn::buf_with_allocator<Instruction>(self->parent->arena);

	// 	mn::buf_push(self->basic_blocks, bb);
	// 	return bb;
	// }

	// inline static Value*
	// basic_block_iadd(Basic_Block* self, Value* a, Value* b)
	// {
	// 	Instruction ins{};
	// 	ins.op = Instruction::Op_IAdd;
	// 	ins.as_iadd.op1 = a;
	// 	ins.as_iadd.op2 = b;
	// 	ins.as_iadd.res = module_value_new(self->parent->parent, a->type);

	// 	mn::buf_push(self->instructions, ins);
	// 	return ins.as_iadd.res;
	// }

	// inline static void
	// basic_block_return(Basic_Block* self, Value* res)
	// {
	// 	Instruction ins{};
	// 	ins.op = Instruction::Op_ReturnValue;
	// 	ins.as_return.res = res;

	// 	mn::buf_push(self->instructions, ins);
	// }
}