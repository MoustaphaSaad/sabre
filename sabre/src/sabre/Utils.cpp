#include "sabre/Utils.h"
#include "sabre/Unit.h"

#include <mn/Path.h>
#include <mn/Defer.h>

namespace sabre
{
	// API
	mn::Result<mn::Str, mn::Err>
	scan_file(const mn::Str& filepath, const mn::Str& fake_path)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath);
		mn_defer(unit_free(unit));

		if (unit_scan(unit))
			return unit_dump_tokens(unit);
		else
			return unit_dump_errors(unit);
	}
}