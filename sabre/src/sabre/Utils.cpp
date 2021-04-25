#include "sabre/Utils.h"
#include "sabre/Unit.h"
#include "sabre/Parse.h"
#include "sabre/AST_Printer.h"
#include "sabre/AST.h"
#include "sabre/GLSL.h"

#include <mn/Path.h>
#include <mn/Defer.h>

namespace sabre
{
	// API
	mn::Result<mn::Str, mn::Err>
	scan_file(const mn::Str& filepath, const mn::Str&)
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

		if (fake_path.count > 0)
		{
			mn::str_free(unit->filepath);
			unit->filepath = mn::str_clone(fake_path);
		}

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		auto parser = parser_new(unit);
		mn_defer(parser_free(parser));

		auto expr = parser_parse_expr(parser);
		if (expr == nullptr)
			return unit_dump_errors(unit);

		if (unit->errs.count > 0)
			return unit_dump_errors(unit);

		auto printer = ast_printer_new();
		mn_defer(ast_printer_free(printer));

		ast_printer_print_expr(printer, expr);

		return ast_printer_str(printer);
	}

	mn::Result<mn::Str, mn::Err>
	parse_stmt_from_file(const mn::Str& filepath, const mn::Str& fake_path)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath);
		mn_defer(unit_free(unit));

		if (fake_path.count > 0)
		{
			mn::str_free(unit->filepath);
			unit->filepath = mn::str_clone(fake_path);
		}

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		auto parser = parser_new(unit);
		mn_defer(parser_free(parser));

		auto stmt = parser_parse_stmt(parser);
		if (stmt == nullptr)
			return unit_dump_errors(unit);

		if (unit->errs.count > 0)
			return unit_dump_errors(unit);

		auto printer = ast_printer_new();
		mn_defer(ast_printer_free(printer));

		ast_printer_print_stmt(printer, stmt);

		return ast_printer_str(printer);
	}

	mn::Result<mn::Str, mn::Err>
	parse_decl_from_file(const mn::Str& filepath, const mn::Str& fake_path)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath);
		mn_defer(unit_free(unit));

		if (fake_path.count > 0)
		{
			mn::str_free(unit->filepath);
			unit->filepath = mn::str_clone(fake_path);
		}

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		auto parser = parser_new(unit);
		mn_defer(parser_free(parser));

		auto decl = parser_parse_decl(parser);
		if (decl == nullptr)
			return unit_dump_errors(unit);

		if (unit->errs.count > 0)
			return unit_dump_errors(unit);

		auto printer = ast_printer_new();
		mn_defer(ast_printer_free(printer));

		ast_printer_print_decl(printer, decl);

		return ast_printer_str(printer);
	}

	mn::Result<mn::Str, mn::Err>
	check_file(const mn::Str& filepath, const mn::Str& fake_path)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath);
		mn_defer(unit_free(unit));

		if (fake_path.count > 0)
		{
			mn::str_free(unit->filepath);
			unit->filepath = mn::str_clone(fake_path);
		}

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		if (unit_parse(unit) == false)
			return unit_dump_errors(unit);

		unit_check(unit);
		return unit_dump_errors(unit);
	}

	mn::Result<mn::Str, mn::Err>
	glsl_gen_expr_from_file(const mn::Str& filepath, const mn::Str& fake_path)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath);
		mn_defer(unit_free(unit));

		if (fake_path.count > 0)
		{
			mn::str_free(unit->filepath);
			unit->filepath = mn::str_clone(fake_path);
		}

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		auto parser = parser_new(unit);
		mn_defer(parser_free(parser));

		auto expr = parser_parse_expr(parser);
		if (expr == nullptr)
			return unit_dump_errors(unit);

		auto stream = mn::memory_stream_new();
		mn_defer(mn::memory_stream_free(stream));

		auto glsl = glsl_new(unit, stream);
		mn_defer(glsl_free(glsl));

		glsl_expr_gen(glsl, expr);

		return mn::memory_stream_str(stream);
	}
}