#pragma once

#include "sabre/Exports.h"
#include "sabre/Tkn.h"
#include "sabre/Err.h"
#include "sabre/Scope.h"

#include <mn/Str.h>
#include <mn/Buf.h>
#include <mn/Str_Intern.h>
#include <mn/Map.h>
#include <mn/Result.h>
#include <mn/Log.h>
#include <mn/Path.h>

namespace sabre
{
	struct Scope;
	struct Unit_File;
	struct Unit_Package;
	struct Unit;
	struct Decl;
	struct Type_Interner;
	struct Symbol;
	struct Type;
	struct Scope;

	// this is a list of constant strings that's used across stages
	// it's placed here so that when we use them as map keys we don't
	// need to intern them before doing the search instead we use
	// these global variables directly because the string interner itself
	// uses these pointers to represent these string values
	inline constexpr const char* KEYWORD_UNIFORM = "uniform";
	inline constexpr const char* KEYWORD_BUILTIN = "builtin";
	inline constexpr const char* KEYWORD_BINDING = "binding";
	inline constexpr const char* KEYWORD_VERTEX = "vertex";
	inline constexpr const char* KEYWORD_PIXEL = "pixel";
	inline constexpr const char* KEYWORD_SV_POSITION = "system_position";
	inline constexpr const char* KEYWORD_SV_DEPTH = "system_depth";
	inline constexpr const char* KEYWORD_SV_PRIMITIVE_ID = "system_primitive_id";
	inline constexpr const char* KEYWORD_SV_THREAD_ID = "system_thread_id";
	inline constexpr const char* KEYWORD_SV_GROUP_ID = "system_group_id";
	inline constexpr const char* KEYWORD_GLSL = "glsl";
	inline constexpr const char* KEYWORD_REFLECT = "reflect";
	inline constexpr const char* KEYWORD_HLSL = "hlsl";
	inline constexpr const char* KEYWORD_HLSL_METHOD = "hlsl_method";
	inline constexpr const char* KEYWORD_SAMPLER_STATE = "sampler_state";
	inline constexpr const char* KEYWORD_SAMPLE_FUNC = "sample_func";
	inline constexpr const char* KEYWORD_GEOMETRY = "geometry";
	inline constexpr const char* KEYWORD_MAX_VERTEX_COUNT = "max_vertex_count";
	inline constexpr const char* KEYWORD_IN = "in";
	inline constexpr const char* KEYWORD_OUT = "out";
	inline constexpr const char* KEYWORD_INOUT = "inout";
	inline constexpr const char* KEYWORD_POINT = "point";
	inline constexpr const char* KEYWORD_LINE = "line";
	inline constexpr const char* KEYWORD_TRIANGLE = "triangle";
	inline constexpr const char* KEYWORD_READ_WRITE = "read_write";
	inline constexpr const char* KEYWORD_COMPUTE = "compute";
	inline constexpr const char* KEYWORD_X = "x";
	inline constexpr const char* KEYWORD_Y = "y";
	inline constexpr const char* KEYWORD_Z = "z";
	inline constexpr const char* KEYWORD_BUILD_BACKEND = "build_backend";
	inline constexpr const char* KEYWORD_BUFFER = "buffer";
	inline constexpr const char* KEYWORD_IMAGE = "image";

	enum COMPILATION_STAGE
	{
		COMPILATION_STAGE_NONE,
		COMPILATION_STAGE_FAILED,
		COMPILATION_STAGE_SCAN,
		COMPILATION_STAGE_PARSE,
		COMPILATION_STAGE_CHECK,
		COMPILATION_STAGE_CODEGEN,
		COMPILATION_STAGE_SUCCESS
	};

	// represents a file compilation unit
	struct Unit_File
	{
		// the package which this file belong to
		Unit_Package* parent_package;
		// name of the package which this file belongs to
		Tkn package_name;
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
		// contains the symbols defined in this file
		Scope* file_scope;
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
	unit_file_resolve_package(Unit_File* self, const mn::Str& path);

	// determine the mode of this compilation unit
	enum COMPILATION_MODE
	{
		COMPILATION_MODE_LIBRARY,
		COMPILATION_MODE_VERTEX,
		COMPILATION_MODE_PIXEL,
		COMPILATION_MODE_GEOMETRY,
		COMPILATION_MODE_COMPUTE,
	};

	struct Input_Layout_Attribute
	{
		Type* type;
		Tag_Table* tags;
	};

	// represents an entry point, along with its symbol and mode (vertex, pixel, geometry, compute, etc...)
	struct Entry_Point
	{
		COMPILATION_MODE mode;
		Symbol* symbol;
		mn::Buf<Symbol*> reachable_symbols;
		mn::Map<const char*, Input_Layout_Attribute> input_layout;
		mn::Buf<Symbol*> uniforms;
		mn::Buf<Symbol*> textures;
		mn::Buf<Symbol*> samplers;
		mn::Buf<Symbol*> buffers;
		mn::Buf<Symbol*> images;
	};

	// creates a new entry point instance
	inline static Entry_Point*
	entry_point_new(Symbol* symbol, COMPILATION_MODE mode)
	{
		auto self = mn::alloc_zerod<Entry_Point>();
		self->mode = mode;
		self->symbol = symbol;
		return self;
	}

	// frees the given entry point instance
	inline static void
	entry_point_free(Entry_Point* self)
	{
		if (self)
		{
			mn::buf_free(self->reachable_symbols);
			mn::map_free(self->input_layout);
			mn::buf_free(self->uniforms);
			mn::buf_free(self->textures);
			mn::buf_free(self->samplers);
			mn::buf_free(self->buffers);
			mn::buf_free(self->images);
			mn::free(self);
		}
	}

	// destruct overload for entry point
	inline static void
	destruct(Entry_Point* self)
	{
		entry_point_free(self);
	}

	// calculates the list of reachable symbols from this entry point, this is done before codegen
	SABRE_EXPORT void
	entry_point_calc_reachable_list(Entry_Point* entry);

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
		// package name, which is the name defined inside the files of this package
		Tkn name;
		// absolute path of this package
		mn::Str absolute_path;
		// files of this package
		mn::Buf<Unit_File*> files;
		// map from absolute file paths to unit files
		mn::Map<mn::Str, Unit_File*> absolute_path_to_file;
		// package level errors
		mn::Buf<Err> errs;
		// all the found entry points in this package
		mn::Buf<Entry_Point*> entry_points;
		// reachable symbols sorted by first usage
		mn::Buf<Symbol*> reachable_symbols;
		// all the symbols are allocated from this arena, so we don't need to manage
		// memory for the symbols on a symbol by symbol basis
		mn::memory::Arena* symbols_arena;
		// global scope of the unit
		Scope* global_scope;
		// list of all the imported packages
		mn::Buf<Unit_Package*> imported_packages;
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
	SABRE_EXPORT bool
	unit_package_add_file(Unit_Package* self, Unit_File* file);

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
	unit_package_resolve_package(Unit_Package* self, const mn::Str& absolute_path);

	// finds the entry point by name, returns nullptr if it doesn't exist
	SABRE_EXPORT Entry_Point*
	unit_package_entry_find(Unit_Package* self, const mn::Str& name);

	// finds the entry point by name, returns nullptr if it doesn't exist
	inline static Entry_Point*
	unit_package_entry_find(Unit_Package* self, const char* name)
	{
		return unit_package_entry_find(self, mn::str_lit(name));
	}

	enum BACKEND_MODE
	{
		BACKEND_MODE_NONE,
		BACKEND_MODE_GLSL,
		BACKEND_MODE_HLSL,
		BACKEND_MODE_SPIRV,
	};

	struct Unit
	{
		// used to intern strings, usually token strings
		mn::Str_Intern str_interner;
		// all the types live here, it makes it simple to manage this memory and compare types
		// because it works just like string interning where pointer == pointer if
		// the content is the same
		Type_Interner* type_interner;
		// maps from and AST node to a scope
		mn::Map<void*, Scope*> scope_table;
		// list of imported packages in this compilation unit
		mn::Buf<Unit_Package*> packages;
		// root package is the first package added to the compilation unit, this is the main package provided by user
		Unit_Package* root_package;
		// root file is the first file added to the compilation unit, this is the main file provided by user
		Unit_File* root_file;
		// map from package path to unit package
		mn::Map<mn::Str, Unit_Package*> absolute_path_to_package;
		// reflection information
		// reachable uniforms info
		mn::Map<int, Symbol*> reachable_uniforms;
		// reachable textures info
		mn::Map<int, Symbol*> reachable_textures;
		// reachable samplers info
		mn::Map<int, Symbol*> reachable_samplers;
		// reflected symbols, they should be const because we write their values in json reflection info
		mn::Buf<Symbol*> reflected_symbols;
		// library collections, map from collection name to its path
		mn::Map<mn::Str, mn::Str> library_collections;
		// symbol stack used to resolve symbol dependencies in typechecking
		// TODO: move to some other place later
		mn::Buf<Symbol*> symbol_stack;
		// list of all the uniforms found in a program
		mn::Buf<Symbol*> all_uniforms;
		// backend mode of this unit, it's best to have match the actual backend codegen you'll
		// be using because it's used in compile time if condition to do type checking
		BACKEND_MODE backend;
		// if enabled it will log memory consumption metrics for this unit
		bool log_metrics;
	};

	SABRE_EXPORT Unit*
	unit_from_file(const mn::Str& filepath, const mn::Str& entry, BACKEND_MODE backend);

	inline static Unit*
	unit_from_file(const char* filepath, const mn::Str& entry, BACKEND_MODE backend)
	{
		return unit_from_file(mn::str_lit(filepath), entry, backend);
	}

	SABRE_EXPORT void
	unit_free(Unit* self);

	inline static void
	destruct(Unit* self)
	{
		unit_free(self);
	}

	// adds a library collection
	inline static mn::Err
	unit_add_library_collection(Unit* self, const mn::Str& name, const mn::Str& path)
	{
		if (auto it = mn::map_lookup(self->library_collections, name))
		{
			if (it->value != path)
				return mn::Err{"library collection '{}' already exists and pointing to '{}'", name, path};
			else
				return mn::Err{};
		}

		if (mn::path_is_folder(path) == false)
		{
			return mn::Err{"library collection path '{}' doesn't exist", path};
		}

		mn::map_insert(self->library_collections, clone(name), clone(path));
		return mn::Err{};
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

	// generates reflection information for the given entry point
	SABRE_EXPORT bool
	unit_reflect(Unit* self, Entry_Point* entry);

	// generates reflection information for the given unit and writes them as json
	SABRE_EXPORT mn::Str
	unit_reflection_info_as_json(Unit* self, Entry_Point* entry, mn::Allocator allocator = mn::allocator_top());

	// generates glsl code for the given entry, if it has errors it will return the an error
	// if entry is nullptr it will generate code for the entire library
	SABRE_EXPORT mn::Result<mn::Str>
	unit_glsl(Unit* self, Entry_Point* entry, mn::Allocator allocator = mn::allocator_top());

	// generates hlsl code for the given entry, if it has errors it will return the an error
	// if entry is nullptr it will generate code for the entire library
	SABRE_EXPORT mn::Result<mn::Str>
	unit_hlsl(Unit* self, Entry_Point* entry, mn::Allocator allocator = mn::allocator_top());

	// generates spirv code for the given unit, if it has errors
	// it will return the an error
	SABRE_EXPORT mn::Result<mn::Str>
	unit_spirv(Unit* self, mn::Allocator allocator = mn::allocator_top());

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