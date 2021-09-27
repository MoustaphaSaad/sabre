#pragma once

#include "sabre/Exports.h"

namespace sabre
{
	struct Entry_Point;

	// generates reflection information for the given entry point
	SABRE_EXPORT void
	reflect_package(Entry_Point* entry);
}