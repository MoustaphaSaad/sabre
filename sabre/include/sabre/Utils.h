#pragma once

#include "sabre/Exports.h"

#include <mn/Result.h>

namespace sabre
{
	// loads and lexes a file given its path on disk, fake_path is used for testing
	// when you want to make the path uniform across testing environment
	SABRE_EXPORT mn::Result<mn::Str, mn::Err>
	scan_file(const mn::Str& filepath, const mn::Str& fake_path);

	// loads and parses an expression from the given file, fake_path is used for testing
	// when you want to make the path uniform across testing environment
	SABRE_EXPORT mn::Result<mn::Str, mn::Err>
	parse_expr_from_file(const mn::Str& filepath, const mn::Str& fake_path);

	// loads and parses a statement from the given file, fake_path is used for testing
	// when you want to make the path uniform across testing environment
	SABRE_EXPORT mn::Result<mn::Str, mn::Err>
	parse_stmt_from_file(const mn::Str& filepath, const mn::Str& fake_path);

	// loads and parses a declaration from the given file, fake_path is used for testing
	// when you want to make the path uniform across testing environment
	SABRE_EXPORT mn::Result<mn::Str, mn::Err>
	parse_decl_from_file(const mn::Str& filepath, const mn::Str& fake_path);

	// loads and typechecks a file, fake_path is used for testing
	// when you want to make the path uniform across testing environment
	SABRE_EXPORT mn::Result<mn::Str, mn::Err>
	check_file(const mn::Str& filepath, const mn::Str& fake_path, const mn::Str& entry, const mn::Str& std_path);

	// loads and parses a file, fake_path is used for testing
	// when you want to make the path uniform across testing environment
	SABRE_EXPORT mn::Result<mn::Str, mn::Err>
	glsl_gen_from_file(const mn::Str& filepath, const mn::Str& fake_path, const mn::Str& entry, const mn::Str& std_path);

	// reflects on the given file
	SABRE_EXPORT mn::Result<mn::Str>
	reflect_file(const mn::Str& filepath, const mn::Str& entry, const mn::Str& std_path);
}