#include "sabre/Utils.h"
#include "sabre/Unit.h"
#include "sabre/Parse.h"
#include "sabre/AST_Printer.h"
#include "sabre/AST.h"
#include "sabre/GLSL.h"
#include "sabre/Reflect.h"

#include <mn/Path.h>
#include <mn/Defer.h>
#include <mn/Json.h>

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

	inline static void
	_push_type(mn::Set<Type*>& types, Type* t)
	{
		if (mn::set_lookup(types, t))
			return;

		switch (t->kind)
		{
		case Type::KIND_STRUCT:
		{
			for (const auto& field: t->struct_type.fields)
				_push_type(types, field.type);
			break;
		}
		default:
			break;
		}
		mn::set_insert(types, t);
	}

	// API
	mn::Result<mn::Str, mn::Err>
	scan_file(const mn::Str& filepath, const mn::Str&)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath, mn::str_lit(""), mn::str_lit(""));
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

		auto unit = unit_from_file(filepath, mn::str_lit(""), mn::str_lit(""));
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

		auto unit = unit_from_file(filepath, mn::str_lit(""), mn::str_lit(""));
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

		auto unit = unit_from_file(filepath, mn::str_lit(""), mn::str_lit(""));
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
	check_file(const mn::Str& filepath, const mn::Str& fake_path, const mn::Str& entry, const mn::Str& std_path)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath, entry, std_path);
		mn_defer(unit_free(unit));

		_unit_change_paths(unit, fake_path);

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		if (unit_parse(unit) == false)
			return unit_dump_errors(unit);

		unit_check(unit);
		return unit_dump_errors(unit);
	}

	mn::Result<mn::Str, mn::Err>
	glsl_gen_from_file(const mn::Str& filepath, const mn::Str& fake_path, const mn::Str& entry, const mn::Str& std_path)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath, entry, std_path);
		mn_defer(unit_free(unit));

		_unit_change_paths(unit, fake_path);

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		if (unit_parse(unit) == false)
			return unit_dump_errors(unit);

		if (unit_check(unit) == false)
			return unit_dump_errors(unit);

		auto stream = mn::memory_stream_new();
		mn_defer(mn::memory_stream_free(stream));

		auto glsl = glsl_new(unit->packages[0], stream);
		mn_defer(glsl_free(glsl));

		glsl_gen(glsl);

		return mn::memory_stream_str(stream);
	}

	mn::Result<mn::Str>
	reflect_file(const mn::Str& filepath, const mn::Str& entry, const mn::Str& std_path)
	{
		if (mn::path_is_file(filepath) == false)
			return mn::Err{ "file '{}' not found", filepath };

		auto unit = unit_from_file(filepath, entry, std_path);
		mn_defer(unit_free(unit));

		if (unit_scan(unit) == false)
			return unit_dump_errors(unit);

		if (unit_parse(unit) == false)
			return unit_dump_errors(unit);

		if (unit_check(unit) == false)
			return unit_dump_errors(unit);

		if (unit_reflect(unit) == false)
			return unit_dump_errors(unit);

		auto types = mn::set_with_allocator<Type*>(mn::memory::tmp());

		auto json_entry = mn::json::value_object_new();
		if (unit->entry_symbol)
		{
			mn::json::value_object_insert(json_entry, "name", mn::json::value_string_new(unit->entry_symbol->name));

			auto json_layout = mn::json::value_array_new();
			for (const auto& [attribute_name, attribute_type]: unit->input_layout)
			{
				auto json_attribute = mn::json::value_object_new();
				mn::json::value_object_insert(json_attribute, "name", mn::json::value_string_new(attribute_name));
				mn::json::value_object_insert(json_attribute, "type", mn::json::value_string_new(mn::strf("{}", attribute_type)));
				mn::json::value_array_push(json_layout, json_attribute);

				_push_type(types, attribute_type);
			}
			mn::json::value_object_insert(json_entry, "input_layout", json_layout);
		}

		auto json_uniforms = mn::json::value_array_new();
		for (const auto& uniform: unit->reachable_uniforms)
		{
			auto json_uniform = mn::json::value_object_new();
			mn::json::value_object_insert(json_uniform, "name", mn::json::value_string_new(uniform.symbol->name));
			mn::json::value_object_insert(json_uniform, "binding", mn::json::value_number_new(uniform.binding));
			mn::json::value_object_insert(json_uniform, "type", mn::json::value_string_new(mn::strf("{}", uniform.symbol->type)));
			mn::json::value_array_push(json_uniforms, json_uniform);

			_push_type(types, uniform.symbol->type);
		}

		auto json_types = mn::json::value_array_new();
		for (auto type: types)
		{
			auto json_type = mn::json::value_object_new();
			mn::json::value_object_insert(json_type, "name", mn::json::value_string_new(mn::strf("{}", type)));

			switch (type->kind)
			{
			case Type::KIND_STRUCT:
			{
				auto json_fields = mn::json::value_array_new();
				for (const auto& field: type->struct_type.fields)
				{
					auto json_field = mn::json::value_object_new();
					mn::json::value_object_insert(json_field, "name", mn::json::value_string_new(field.name.str));
					mn::json::value_object_insert(json_field, "type", mn::json::value_string_new(mn::strf("{}", field.type)));
					mn::json::value_array_push(json_fields, json_field);
				}
				mn::json::value_object_insert(json_type, "fields", json_fields);
				break;
			}
			default:
				break;
			}
			mn::json::value_array_push(json_types, json_type);
		}

		auto json_result = mn::json::value_object_new();
		mn_defer(mn::json::value_free(json_result));

		mn::json::value_object_insert(json_result, "entry", json_entry);
		mn::json::value_object_insert(json_result, "uniforms", json_uniforms);
		mn::json::value_object_insert(json_result, "types", json_types);

		return mn::strf("{}", json_result);
	}
}