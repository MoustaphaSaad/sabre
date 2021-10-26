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

	// represents a SPIRV type
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

	inline static bool
	type_is_int(Type* t)
	{
		return t->kind == Type::KIND_INT;
	}

	struct Value
	{
		ID id;
		Type* type;
	};

	// represents an instruction
	struct Instruction
	{
		enum Op
		{
			Op_IAdd,
			Op_ReturnValue,
		};

		Op kind;
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
				Value* value;
			} as_return;
		};
	};

	// represents a SPIRV linear sequence of instructions
	struct Basic_Block
	{
		Func* func;
		ID id;
		mn::Buf<Instruction> instructions;
	};

	// generates the correction add instruction for the given 2 values and
	// returns the output value
	SABRE_EXPORT Value*
	basic_block_add(Basic_Block* self, Value* op1, Value* op2);

	// returns the given value from the given basic block
	SABRE_EXPORT Value*
	basic_block_ret(Basic_Block* self, Value* res);

	// represents a SPIRV function
	struct Func
	{
		Module* module;
		ID id;
		Type* type;
		mn::Buf<Value*> args;
		mn::Buf<Basic_Block*> blocks;
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
}