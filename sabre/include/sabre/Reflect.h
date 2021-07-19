#pragma once

#include "sabre/Exports.h"

namespace sabre
{
	struct Unit_Package;

	// generates reflection information for the given package
	SABRE_EXPORT void
	reflect_package(Unit_Package* unit);
}