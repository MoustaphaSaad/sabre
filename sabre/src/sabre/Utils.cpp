#include "sabre/Utils.h"
#include "sabre/Unit.h"
#include "sabre/Parse.h"
#include "sabre/AST_Printer.h"
#include "sabre/AST.h"

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

	mn::Result<mn::Str, mn::Err>
	parse_expr_from_file(const mn::Str& filepath, const mn::Str& fake_path)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath);
		mn_defer(unit_free(unit));

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		auto parser = parser_new(unit);
		mn_defer(parser_free(parser));

		auto expr = parser_parse_expr(parser);
		if (expr == nullptr)
			return unit_dump_errors(unit);
		mn_defer(expr_free(expr));

		auto printer = ast_printer_new();
		mn_defer(ast_printer_free(printer));

		ast_printer_print_expr(printer, expr);

		return ast_printer_str(printer);
	}
}