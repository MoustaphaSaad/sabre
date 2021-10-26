#pragma once

#include "sabre/Exports.h"

#include <mn/Map.h>
#include <mn/Buf.h>

namespace sabre::spirv
{
	struct Module;
	struct Type;
	struct Func;
	struct Basic_Block;
	struct Value;
}

namespace sabre
{
	struct Unit_Package;
	struct Type;
	struct Scope;

	// value table is something analog to scope but it works on SSA values level
	struct Value_Table
	{
		Value_Table* parent;
		mn::Map<const char*, spirv::Value*> table;
	};

	// constructs a new value table
	SABRE_EXPORT Value_Table*
	value_table_new(Value_Table* parent);

	// frees the given value table
	SABRE_EXPORT void
	value_table_free(Value_Table* self);

	inline static void
	destruct(Value_Table* self)
	{
		value_table_free(self);
	}

	// searches the given value table for a value with the given name
	SABRE_EXPORT spirv::Value*
	value_table_shallow_find(const Value_Table* self, const char* name);

	// searches the given value table and all its parents for a value with the given name
	SABRE_EXPORT spirv::Value*
	value_table_find(const Value_Table* self, const char* name);

	// adds a value with the given name to the value table
	SABRE_EXPORT void
	value_table_add(Value_Table* self, const char* name, spirv::Value* value);

	struct SPIRV
	{
		Unit_Package* unit;
		spirv::Module* out;
		mn::Map<Type*, spirv::Type*> spirv_type_table;
		mn::Map<Scope*, Value_Table*> scope_value_table;
		mn::Buf<Scope*> scope_stack;
		mn::Buf<spirv::Func*> func_stack;
		mn::Buf<spirv::Basic_Block*> bb_stack;
	};

	// creates a new SPIRV generator instance
	SABRE_EXPORT SPIRV
	spirv_new(Unit_Package* unit);

	// free the given SPRIV generator
	SABRE_EXPORT void
	spirv_free(SPIRV& self);

	// it will generate all of the reachable symbols in the given unit
	SABRE_EXPORT void
	spirv_gen(SPIRV& self);
}