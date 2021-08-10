#include <mn/IO.h>
#include <mn/Defer.h>
#include <mn/Path.h>

#include <sabre/Utils.h>

const char* HELP = R"""(sabrec the sabre compiler
sabrec command [options] INPUT...
COMMANDS:
  help: prints this message
  scan: scans the given input files and prints a token stream
  parse-expr: scans the given input files and prints a token stream
  parse-stmt: parses a single statement from the given files and prints it's AST representation
  parse-decl: parses a single declaration from the given files and prints it's AST representation
  check: performs type checking on the given files
  glsl-gen: generates GLSL code from the given files
  reflect: generates reflection information for the given files

OPTIONS:
  -entry: specifies the entry point function of the given program)""";

inline static void
print_help()
{
	mn::print("{}\n", HELP);
}

struct Args
{
	mn::Str cmd;
	mn::Str entry;
	mn::Buf<mn::Str> input;
	mn::Map<mn::Str, mn::Str> collections;
};

inline static void
args_free(Args& self)
{
	mn::str_free(self.cmd);
	mn::str_free(self.entry);
	destruct(self.input);
	destruct(self.collections);
}

inline static bool
args_parse(Args& self, int argc, char** argv)
{
	if (argc < 2)
		return false;

	self.cmd = mn::str_lit(argv[1]);
	for (size_t i = 2; i < argc; ++i)
	{
		auto str = mn::str_lit(argv[i]);
		if (str == "-entry" && i + 1 < argc)
		{
			self.entry = mn::str_lit(argv[i + 1]);
			++i;
		}
		else if (str == "-collection" && i + 1 < argc)
		{
			auto collection_arg = mn::str_lit(argv[i + 1]);
			++i;

			if (collection_arg.count == 0)
			{
				mn::printerr("collection name and path not specified\n");
				return false;
			}

			auto collection_path_pos = mn::str_find(collection_arg, ':', 0);
			if (collection_path_pos == SIZE_MAX)
			{
				mn::printerr("could not find collection name in '{}'\n", collection_arg);
				return false;
			}

			const char* collection_path = collection_arg.ptr + collection_path_pos + 1;
			if (mn::path_is_folder(collection_path) == false)
			{
				mn::printerr("collection path '{}' does not exist\n", collection_path);
				return false;
			}

			auto collection_name = mn::str_from_substr(collection_arg.ptr, collection_arg.ptr + collection_path_pos, mn::memory::tmp());
			if (mn::map_lookup(self.collections, collection_name) != nullptr)
			{
				mn::printerr("collection '{}' already exists\n", collection_path);
				return false;
			}

			mn::map_insert(self.collections, clone(collection_name), mn::str_from_c(collection_path));
		}
		else
		{
			if (mn::path_exists(str) == false)
			{
				mn::printerr("path '{}' does not exist\n", str);
				return false;
			}
			mn::buf_push(self.input, str);
		}
	}
	return true;
}

int main(int argc, char** argv)
{
	auto compiler_folder = mn::path_absolute(mn::file_directory(argv[0], mn::memory::tmp()));
	auto std_library_folder = mn::path_join(compiler_folder, "std");
	mn_defer(mn::str_free(std_library_folder));

	Args args{};
	mn_defer(args_free(args));

	if (args_parse(args, argc, argv) == false)
	{
		print_help();
		return EXIT_FAILURE;
	}

	if (mn::map_lookup(args.collections, mn::str_lit("std")) == nullptr)
	{
		if (mn::path_is_folder(std_library_folder))
			mn::map_insert(args.collections, mn::str_lit("std"), clone(std_library_folder));
	}

	if (args.cmd == "help")
	{
		print_help();
		return EXIT_SUCCESS;
	}
	else if (args.cmd == "scan")
	{
		if (args.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			print_help();
			return EXIT_FAILURE;
		}
		auto path = args.input[0];

		auto [answer, err] = sabre::scan_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return EXIT_SUCCESS;
	}
	else if (args.cmd == "parse-expr")
	{
		if (args.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			print_help();
			return EXIT_FAILURE;
		}
		auto path = args.input[0];

		auto [answer, err] = sabre::parse_expr_from_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return EXIT_SUCCESS;
	}
	else if (args.cmd == "parse-stmt")
	{
		if (args.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			print_help();
			return EXIT_FAILURE;
		}
		auto path = args.input[0];

		auto [answer, err] = sabre::parse_stmt_from_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return EXIT_SUCCESS;
	}
	else if (args.cmd == "parse-decl")
	{
		if (args.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			print_help();
			return EXIT_FAILURE;
		}
		auto path = args.input[0];

		auto [answer, err] = sabre::parse_decl_from_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return EXIT_SUCCESS;
	}
	else if (args.cmd == "check")
	{
		if (args.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			print_help();
			return EXIT_FAILURE;
		}
		auto path = args.input[0];

		auto [answer, err] = sabre::check_file(path, mn::str_lit(""), args.entry, args.collections);
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return EXIT_SUCCESS;
	}
	else if (args.cmd == "glsl-gen")
	{
		if (args.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			print_help();
			return EXIT_FAILURE;
		}
		auto path = args.input[0];

		auto [answer, err] = sabre::glsl_gen_from_file(path, mn::str_lit(""), args.entry, args.collections);
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return EXIT_SUCCESS;
	}
	else if (args.cmd == "reflect")
	{
		if (args.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			print_help();
			return EXIT_FAILURE;
		}
		auto path = args.input[0];

		auto [answer, err] = sabre::reflect_file(path, args.entry, args.collections);
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return EXIT_SUCCESS;
	}
	else
	{
		mn::printerr("invalid command line args\n");
		return EXIT_FAILURE;
	}
}