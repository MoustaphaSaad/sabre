#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"
#include "sabre/Err.h"
#include "sabre/Type_Interner.h"

#include <mn/Str.h>
#include <mn/Buf.h>
#include <mn/Str_Intern.h>
#include <mn/Map.h>
#include <mn/Result.h>

namespace sabre
{
	struct Scope;
	struct Unit_File;
	struct Unit_Package;
	struct Unit;

	enum COMPILATION_STAGE
	{
		COMPILATION_STAGE_NONE,
		COMPILATION_STAGE_FAILED,
		COMPILATION_STAGE_SCAN,
		COMPILATION_STAGE_PARSE,
		COMPILATION_STAGE_CHECK,
		COMPILATION_STAGE_CODEGEN,
	};

	// represents a file compilation unit
	struct Unit_File
	{
		// the package which this file belong to
		Unit_Package* parent_package;
		// absolute path of the file on disk
		mn::Str absolute_path;
		// the path of the file supplied by user
		mn::Str filepath;
		// content of the file
		mn::Str content;
		// errors in this file
		mn::Buf<Err> errs;
		// tokens in this file
		mn::Buf<Tkn> tkns;
		// line ranges
		mn::Buf<Rng> lines;
		// all the AST values are allocated from this arena, so we don't need to manage
		// memory for AST on a node by node basis
		mn::memory::Arena* ast_arena;
		// declarations parsed in this unit
		mn::Buf<Decl*> decls;
	};

	// creates a new unit file from a path
	SABRE_EXPORT Unit_File*
	unit_file_from_path(const mn::Str& path);

	inline static Unit_File*
	unit_file_from_path(const char* path)
	{
		return unit_file_from_path(mn::str_lit(path));
	}

	// frees the given unit file
	SABRE_EXPORT void
	unit_file_free(Unit_File* self);

	inline static void
	destruct(Unit_File* self)
	{
		unit_file_free(self);
	}

	// adds an error to this file unit
	inline static void
	unit_err(Unit_File* self, Err err)
	{
		mn::buf_push(self->errs, err);
	}

	// scans the given file unit, and returns true on success
	SABRE_EXPORT bool
	unit_file_scan(Unit_File* self);

	// parses the given file unit, and returns true on success
	SABRE_EXPORT bool
	unit_file_parse(Unit_File* self);

	// dumps the tokens in this file unit
	SABRE_EXPORT void
	unit_file_dump_tokens(Unit_File* self, mn::Stream out);

	// dumps the erros in this file unit
	SABRE_EXPORT void
	unit_file_dump_errors(Unit_File* self, mn::Stream out);

	// returns whether this unit has errors or not
	inline static bool
	unit_file_has_errors(Unit_File* self)
	{
		if (self->errs.count > 0)
			return true;
		return false;
	}

	// processes the package given its path
	SABRE_EXPORT mn::Result<Unit_Package*>
	unit_file_resolve_package(Unit_File* self, const mn::Str& path, Tkn name);

	// represents a package compilation unit
	struct Unit_Package
	{
		enum STATE
		{
			// resolved = parsed
			STATE_UNRESOLVED,
			STATE_RESOLVING,
			STATE_RESOLVED
		};

		// state of the given package, useful to detect cyclic imports
		STATE state;
		// stage which this package is currently in, scan, parse, check, etc...
		COMPILATION_STAGE stage;
		// parent compilation which this package belongs to
		Unit* parent_unit;
		// absolute path of this package
		mn::Str absolute_path;
		// files of this package
		mn::Buf<Unit_File*> files;
		// map from absolute file paths to unit files
		mn::Map<mn::Str, Unit_File*> absolute_path_to_file;
		// package level errors
		mn::Buf<Err> errs;
		// reachable symbols sorted by first usage
		mn::Buf<Symbol*> reachable_symbols;
		// all the symbols are allocated from this arena, so we don't need to manage
		// memory for the symbols on a symbol by symbol basis
		mn::memory::Arena* symbols_arena;
		// global scope of the unit
		Scope* global_scope;
		// maps from package name (user defined) to package pointer
		mn::Map<const char*, Unit_Package*> imported_packages;
	};

	// creates a new package compilation unit
	SABRE_EXPORT Unit_Package*
	unit_package_new();

	// frees the given unit package
	SABRE_EXPORT void
	unit_package_free(Unit_Package* self);

	inline static void
	destruct(Unit_Package* self)
	{
		unit_package_free(self);
	}

	// adds an error to this file unit
	inline static void
	unit_err(Unit_Package* self, Err err)
	{
		mn::buf_push(self->errs, err);
	}

	// adds the given file to the package
	inline static bool
	unit_package_add_file(Unit_Package* self, Unit_File* file)
	{
		if (mn::map_lookup(self->absolute_path_to_file, file->absolute_path) != nullptr)
			return false;

		file->parent_package = self;
		mn::buf_push(self->files, file);
		mn::map_insert(self->absolute_path_to_file, file->absolute_path, file);
		return true;
	}

	// scans the given package unit, and returns true on success
	SABRE_EXPORT bool
	unit_package_scan(Unit_Package* self);

	// parses the given package unit, and returns true on success
	SABRE_EXPORT bool
	unit_package_parse(Unit_Package* self);

	// checks the given package unit, and returns true on success
	SABRE_EXPORT bool
	unit_package_check(Unit_Package* self);

	// dumps the tokens in this package unit
	SABRE_EXPORT void
	unit_package_dump_tokens(Unit_Package* self, mn::Stream out);

	// dumps the erros in this package unit
	SABRE_EXPORT void
	unit_package_dump_errors(Unit_Package* self, mn::Stream out);

	// returns whether this unit has errors or not
	inline static bool
	unit_package_has_errors(Unit_Package* self)
	{
		if (self->errs.count > 0)
			return true;

		for (auto file: self->files)
			if (unit_file_has_errors(file))
				return true;
		return false;
	}

	// processes the package given its path
	SABRE_EXPORT mn::Result<Unit_Package*>
	unit_package_resolve_package(Unit_Package* self, const mn::Str& absolute_path, Tkn name);

	struct Unit
	{
		// used to intern strings, usually token strings
		mn::Str_Intern str_interner;
		// all the types live here, it makes it simple to manage this memory and compare types
		// because it works just like string interning where pointer == pointer if
		// the content is the same
		Type_Interner type_interner;
		// maps from and AST node to a scope
		mn::Map<void*, Scope*> scope_table;
		// list of imported packages in this compilation unit
		mn::Buf<Unit_Package*> packages;
		// map from package path to unit package
		mn::Map<mn::Str, Unit_Package*> absolute_path_to_package;
	};

	SABRE_EXPORT Unit*
	unit_from_file(const mn::Str& filepath);

	inline static Unit*
	unit_from_file(const char* filepath)
	{
		return unit_from_file(mn::str_lit(filepath));
	}

	SABRE_EXPORT void
	unit_free(Unit* self);

	inline static void
	destruct(Unit* self)
	{
		unit_free(self);
	}

	// adds a given package to the compilation unit
	inline static bool
	unit_add_package(Unit* self, Unit_Package* package)
	{
		if (mn::map_lookup(self->absolute_path_to_package, package->absolute_path) != nullptr)
			return false;

		package->parent_unit = self;
		mn::buf_push(self->packages, package);
		mn::map_insert(self->absolute_path_to_package, package->absolute_path, package);
		return true;
	}

	// scans the given unit and returns whether it finished correctly
	SABRE_EXPORT bool
	unit_scan(Unit* self);

	// parses the given unit and returns whether it finishes correctly
	SABRE_EXPORT bool
	unit_parse(Unit* self);

	// typecheckes the given unit and returns whether it finishes correctly
	SABRE_EXPORT bool
	unit_check(Unit* self);

	// dumps all the scanned tokens to a string
	SABRE_EXPORT mn::Str
	unit_dump_tokens(Unit* self, mn::Allocator allocator = mn::allocator_top());

	// dumps all the errors to a string
	SABRE_EXPORT mn::Str
	unit_dump_errors(Unit* self, mn::Allocator allocator = mn::allocator_top());

	// interns a range of string
	inline static const char*
	unit_intern(Unit* self, const char* begin_it, const char* end_it)
	{
		return mn::str_intern(self->str_interner, begin_it, end_it);
	}

	// interns a null terminated string
	inline static const char*
	unit_intern(Unit* self, const char* str)
	{
		return mn::str_intern(self->str_interner, str);
	}

	// interns a string range into a unit file
	inline static const char*
	unit_intern(Unit_File* self, const char* begin_it, const char* end_it)
	{
		return unit_intern(self->parent_package->parent_unit, begin_it, end_it);
	}

	// interns a null terminated string into a unit file
	inline static const char*
	unit_intern(Unit_File* self, const char* str)
	{
		return unit_intern(self->parent_package->parent_unit, str);
	}

	// searchs for the scope associated with the given ptr, and creates a new one if it doesn't exist
	SABRE_EXPORT Scope*
	unit_create_scope_for(Unit* self, void* ptr, Scope* parent, const char* name, Type* expected_type, Scope::FLAG flags);

	// searchs for the scope associated with the given ptr, and creates a new one if it doesn't exist
	inline static Scope*
	unit_create_scope_for(Unit_Package* self, void* ptr, Scope* parent, const char* name, Type* expected_type, Scope::FLAG flags)
	{
		return unit_create_scope_for(self->parent_unit, ptr, parent, name, expected_type, flags);
	}

	// searchs and returns the associated scope of the given key/ptr, if it doesn't exist it will return nullptr
	inline static Scope*
	unit_scope_find(Unit* self, void* ptr)
	{
		if (auto it = mn::map_lookup(self->scope_table, ptr))
			return it->value;
		return nullptr;
	}

	// returns whether this unit has errors or not
	inline static bool
	unit_has_errors(Unit* self)
	{
		for (auto package: self->packages)
			if (unit_package_has_errors(package))
				return true;
		return false;
	}

	// processes the package given its path and either returns an error or the package name
	SABRE_EXPORT mn::Result<Unit_Package*>
	unit_resolve_package(Unit* self, const mn::Str& absolute_path);
}