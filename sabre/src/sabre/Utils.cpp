#include "sabre/Utils.h"
#include "sabre/Unit.h"
#include "sabre/Parse.h"
#include "sabre/AST_Printer.h"
#include "sabre/AST.h"
#include "sabre/GLSL.h"
#include "sabre/Reflect.h"

#include <mn/Path.h>
#include <mn/Defer.h>

namespace sabre
{
	inline static void
	_unit_change_paths(Unit* self, const mn::Str& fake_path)
	{
		if (fake_path.count == 0)
			return;

		for (auto package: self->packages)
		{
			for (auto file: package->files)
			{
				mn::str_free(file->filepath);
				file->filepath = clone(fake_path);
			}
		}
	}

	// API
	mn::Result<mn::Str, mn::Err>
	scan_file(const mn::Str& filepath, const mn::Str&)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath, mn::str_lit(""));
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

		auto unit = unit_from_file(filepath, mn::str_lit(""));
		mn_defer(unit_free(unit));

		_unit_change_paths(unit, fake_path);

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		auto parser = parser_new(unit->packages[0]->files[0]);
		mn_defer(parser_free(parser));

		auto expr = parser_parse_expr(parser);
		if (expr == nullptr)
			return unit_dump_errors(unit);

		if (unit_has_errors(unit))
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

		auto unit = unit_from_file(filepath, mn::str_lit(""));
		mn_defer(unit_free(unit));

		_unit_change_paths(unit, fake_path);

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		auto parser = parser_new(unit->packages[0]->files[0]);
		mn_defer(parser_free(parser));

		auto stmt = parser_parse_stmt(parser);
		if (stmt == nullptr)
			return unit_dump_errors(unit);

		if (unit_has_errors(unit))
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

		auto unit = unit_from_file(filepath, mn::str_lit(""));
		mn_defer(unit_free(unit));

		_unit_change_paths(unit, fake_path);

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		auto parser = parser_new(unit->packages[0]->files[0]);
		mn_defer(parser_free(parser));

		auto decl = parser_parse_decl(parser);
		if (decl == nullptr)
			return unit_dump_errors(unit);

		if (unit_has_errors(unit))
			return unit_dump_errors(unit);

		auto printer = ast_printer_new();
		mn_defer(ast_printer_free(printer));

		ast_printer_print_decl(printer, decl);

		return ast_printer_str(printer);
	}

	mn::Result<mn::Str, mn::Err>
	check_file(const mn::Str& filepath, const mn::Str& fake_path, const mn::Str& entry, const mn::Map<mn::Str, mn::Str>& library_collections)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath, entry);
		mn_defer(unit_free(unit));

		for (const auto& [name, path]: library_collections)
			if (auto err = unit_add_library_collection(unit, name, path))
				return err;

		_unit_change_paths(unit, fake_path);

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		if (unit_parse(unit) == false)
			return unit_dump_errors(unit);

		unit_check(unit);
		return unit_dump_errors(unit);
	}

	mn::Result<mn::Str, mn::Err>
	glsl_gen_from_file(const mn::Str& filepath, const mn::Str& fake_path, const mn::Str& entry, const mn::Map<mn::Str, mn::Str>& library_collections)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath, entry);
		mn_defer(unit_free(unit));

		for (const auto& [name, path]: library_collections)
			if (auto err = unit_add_library_collection(unit, name, path))
				return err;

		_unit_change_paths(unit, fake_path);

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		if (unit_parse(unit) == false)
			return unit_dump_errors(unit);

		if (unit_check(unit) == false)
			return unit_dump_errors(unit);

		return unit_glsl(unit);
	}

	mn::Result<mn::Str, mn::Err>
	hlsl_gen_from_file(const mn::Str& filepath, const mn::Str& fake_path, const mn::Str& entry, const mn::Map<mn::Str, mn::Str>& library_collections)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath, entry);
		mn_defer(unit_free(unit));

		for (const auto& [name, path]: library_collections)
			if (auto err = unit_add_library_collection(unit, name, path))
				return err;

		_unit_change_paths(unit, fake_path);

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		if (unit_parse(unit) == false)
			return unit_dump_errors(unit);

		if (unit_check(unit) == false)
			return unit_dump_errors(unit);

		return unit_hlsl(unit);
	}

	mn::Result<mn::Str>
	reflect_file(const mn::Str& filepath, const mn::Str& entry, const mn::Map<mn::Str, mn::Str>& library_collections)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath, entry);
		mn_defer(unit_free(unit));

		for (const auto& [name, path]: library_collections)
			if (auto err = unit_add_library_collection(unit, name, path))
				return err;

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		if (unit_parse(unit) == false)
			return unit_dump_errors(unit);

		if (unit_check(unit) == false)
			return unit_dump_errors(unit);

		if (unit_reflect(unit) == false)
			return unit_dump_errors(unit);

		return unit_reflection_info_as_json(unit);
	}
}