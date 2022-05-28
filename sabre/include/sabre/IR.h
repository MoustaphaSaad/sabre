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

	union Constant
	{
		int as_int;
		bool as_bool;
	};

	// represents a spirv entity, it can hold types, functions, etc...
	struct Entity
	{
		enum KIND
		{
			KIND_TYPE,
			KIND_FUNC,
			KIND_BASIC_BLOCK,
			KIND_VALUE,
			KIND_CONSTANT,
		};

		KIND kind;
		union
		{
			Type* as_type;
			Func* as_func;
			Basic_Block* as_basic_block;
			Value* as_value;
			struct
			{
				Value* value;
				Constant data;
			} as_constant;
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

	// wraps the given constant int in an entity
	inline static Entity
	entity_from_constant(Value* value, int data)
	{
		Entity self{};
		self.kind = Entity::KIND_CONSTANT;
		self.as_constant.value = value;
		self.as_constant.data.as_int = data;
		return self;
	}

	// wraps the given constant bool in an entity
	inline static Entity
	entity_from_constant(Value* value, bool data)
	{
		Entity self{};
		self.kind = Entity::KIND_CONSTANT;
		self.as_constant.value = value;
		self.as_constant.data.as_bool = data;
		return self;
	}

	enum STORAGE_CLASS
	{
		STORAGE_CLASS_FUNCTION,
	};

	// represents a SPIRV type
	struct Type
	{
		enum KIND
		{
			KIND_VOID,
			KIND_BOOL,
			KIND_INT,
			KIND_FUNC,
			KIND_PTR,
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

			struct
			{
				Type* base;
				STORAGE_CLASS storage_class;
			} as_ptr;
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
			Op_ISub,
			Op_IMul,
			Op_SDiv,
			Op_BitwiseAnd,
			Op_IEqual,
			Op_Variable,
			Op_Load,
			Op_Store,
			Op_ReturnValue,
			Op_SelectionMerge,
			Op_BranchConditional,
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
				Value* op1;
				Value* op2;
				Value* res;
			} as_isub;

			struct
			{
				Value* op1;
				Value* op2;
				Value* res;
			} as_imul;

			struct
			{
				Value* op1;
				Value* op2;
				Value* res;
			} as_sdiv;

			struct
			{
				Value* op1;
				Value* op2;
				Value* res;
			} as_bitwise_and;

			struct
			{
				Value* op1;
				Value* op2;
				Value* res;
			} as_iequal;

			struct
			{
				Type* type;
				STORAGE_CLASS storage_class;
				Value* init;
				Value* res;
			} as_variable;

			struct
			{
				Type* type;
				Value* src;
				Value* res;
			} as_load;

			struct
			{
				Value* src;
				Value* dst;
			} as_store;

			struct
			{
				Value* value;
			} as_return;

			struct
			{
				Basic_Block* merge_branch;
			} as_selection_merge;

			struct
			{
				Value* cond;
				Basic_Block* true_branch;
				Basic_Block* false_branch;
			} as_branch_conditional;
		};
	};

	// represents a SPIRV linear sequence of instructions
	struct Basic_Block
	{
		Func* func;
		ID id;
		mn::Buf<Instruction> instructions;
		bool terminated;
	};

	// generates add instruction for the given 2 values and
	// returns the output value
	SABRE_EXPORT Value*
	basic_block_add(Basic_Block* self, Value* op1, Value* op2);

	// generates sub instruction for the given 2 values and returns the output value
	SABRE_EXPORT Value*
	basic_block_sub(Basic_Block* self, Value* op1, Value* op2);

	// generates mul instruction for the given 2 values and returns the output value
	SABRE_EXPORT Value*
	basic_block_mul(Basic_Block* self, Value* op1, Value* op2);

	// generates div instruction for the given 2 values and returns the output value
	SABRE_EXPORT Value*
	basic_block_div(Basic_Block* self, Value* op1, Value* op2);

	// generates bitwise and instruction for the given 2 values and returns the output value
	SABRE_EXPORT Value*
	basic_block_bitwise_and(Basic_Block* self, Value* op1, Value* op2);

	// generates equal instruction for the given 2 values and returns the output value
	SABRE_EXPORT Value*
	basic_block_equal(Basic_Block* self, Value* op1, Value* op2);

	// returns the given value from the given basic block
	SABRE_EXPORT Value*
	basic_block_ret(Basic_Block* self, Value* res);

	// creates a new variable with the given type
	SABRE_EXPORT Value*
	basic_block_variable(Basic_Block* self, Type* type, STORAGE_CLASS storage_class, Value* init);

	// loads data with the given type from the given source
	SABRE_EXPORT Value*
	basic_block_load(Basic_Block* self, Type* type, Value* src);

	// stores data from src to dst
	SABRE_EXPORT void
	basic_block_store(Basic_Block* self, Value* src, Value* dst);

	// branches conditionally on the given value either to true branch or false branch
	SABRE_EXPORT void
	basic_block_branch(Basic_Block* self, Value* cond, Basic_Block* true_branch, Basic_Block* false_branch);

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

	// creates a new bool type instance
	SABRE_EXPORT Type*
	module_type_bool_new(Module* self);

	// creates a new int type instance
	SABRE_EXPORT Type*
	module_type_int_new(Module* self, int bit_width, bool is_signed);

	// creates a new pointer to type instance
	SABRE_EXPORT Type*
	module_type_pointer_new(Module* self, Type* base, STORAGE_CLASS storage_class);

	// creates a new func type instance
	SABRE_EXPORT Type*
	module_type_func_new(Module* self, Type* return_type);

	// adds argument to function type
	SABRE_EXPORT void
	module_type_func_arg(Type* func, Type* arg);

	// creates a new constant integer value
	SABRE_EXPORT Value*
	module_int_constant(Module* self, Type* type, int value);

	// creates a new constant bool value
	SABRE_EXPORT Value*
	module_bool_constant(Module* self, bool value);

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