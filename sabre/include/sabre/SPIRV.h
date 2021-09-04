#pragma once

#include "sabre/Exports.h"

#include <mn/Map.h>

namespace sabre::spirv
{
	struct Module;
	struct Type;
}

namespace sabre
{
	struct Unit_Package;
	struct Type;

	struct SPIRV
	{
		Unit_Package* unit;
		spirv::Module* out;
		mn::Map<Type*, spirv::Type*> spirv_type_table;
	};

	// creates a new SPIRV generator instance
	SABRE_EXPORT SPIRV
	spirv_new(Unit_Package* unit);

	// freew the given SPRIV generator
	SABRE_EXPORT void
	spirv_free(SPIRV& self);

	// it will generate all of the reachable symbols in the given unit
	SABRE_EXPORT void
	spirv_gen(SPIRV& self);
}