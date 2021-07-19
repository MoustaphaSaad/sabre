#include <mn/IO.h>
#include <mn/Defer.h>
#include <mn/Path.h>

#include <sabre/Utils.h>

struct CLI_Option
{
	mn::Str name;
	mn::Str desc;
};

inline static CLI_Option
cli_option_new()
{
	return CLI_Option{};
}

inline static void
cli_option_free(CLI_Option& self)
{
	mn::str_free(self.name);
	mn::str_free(self.desc);
}

inline static void
destruct(CLI_Option& self)
{
	cli_option_free(self);
}

struct CLI_Command
{
	mn::Str name;
	mn::Str desc;
};

inline static CLI_Command
cli_command_new(mn::Allocator allocator = mn::allocator_top())
{
	return CLI_Command{};
}

inline static void
cli_command_free(CLI_Command& self)
{
	mn::str_free(self.name);
}

inline static void
destruct(CLI_Command& self)
{
	cli_command_free(self);
}

struct CLI
{
	mn::Str desc;
	mn::Map<mn::Str, CLI_Command> commands_schema;
	mn::Map<mn::Str, CLI_Option> options_schema;

	mn::Str executable_name;
	mn::Str cmd;
	mn::Map<mn::Str, mn::Str> options;
	mn::Buf<mn::Str> input;
};

inline static CLI
cli_new(const mn::Str& desc, mn::Allocator allocator = mn::allocator_top())
{
	CLI self{};
	self.desc = mn::str_clone(desc, allocator);
	self.commands_schema = mn::map_with_allocator<mn::Str, CLI_Command>(allocator);
	self.options_schema = mn::map_with_allocator<mn::Str, CLI_Option>(allocator);

	self.options = mn::map_with_allocator<mn::Str, mn::Str>(allocator);
	self.input = mn::buf_with_allocator<mn::Str>(allocator);
	// we don't fill the other members because they are filled by the argv strings which are constant
	return self;
}

inline static void
cli_free(CLI& self)
{
	mn::str_free(self.desc);
	destruct(self.commands_schema);
	destruct(self.options_schema);
	mn::str_free(self.executable_name);
	mn::str_free(self.cmd);
	destruct(self.options);
	destruct(self.input);
}

inline static void
destruct(CLI& self)
{
	cli_free(self);
}

inline static void
cli_command_add(CLI& self, const mn::Str& name, const mn::Str& desc)
{
	assert(mn::map_lookup(self.commands_schema, name) == nullptr);
	auto command = cli_command_new();
	command.name = mn::str_clone(name, self.commands_schema.values.allocator);
	command.desc = mn::str_clone(desc, self.commands_schema.values.allocator);
	mn::map_insert(self.commands_schema, command.name, command);
}

inline static void
cli_option_add(CLI& self, const mn::Str& name, const mn::Str& desc)
{
	assert(mn::map_lookup(self.options_schema, name) == nullptr);

	auto option = cli_option_new();
	option.name = mn::str_clone(name, self.commands_schema.values.allocator);
	option.desc = mn::str_clone(desc, self.commands_schema.values.allocator);
	mn::map_insert(self.options_schema, option.name, option);
}

inline static mn::Err
cli_parse(CLI& self, int argc, char** argv)
{
	int i = 0;
	self.executable_name = mn::str_lit(argv[0]);
	++i;

	if (i >= argc)
		return mn::Err{};

	self.cmd = mn::str_lit(argv[i]);
	auto it = mn::map_lookup(self.commands_schema, self.cmd);
	if (it == nullptr)
		return mn::Err{ "command '{}' is not supported", self.cmd };
	auto& cmd = it->value;

	for (++i; i < argc; ++i)
	{
		auto str = mn::str_lit(argv[i]);
		auto it = mn::map_lookup(self.options_schema, str);
		if (it == nullptr)
		{
			mn::buf_push(self.input, str);
		}
		else if (i + 1 < argc)
		{
			mn::map_insert(self.options, str, mn::str_lit(argv[i + 1]));
			++i;
		}
	}
	return mn::Err{};
}

inline static mn::Str
cli_option_value(CLI& self, const mn::Str& name)
{
	if (auto it = mn::map_lookup(self.options, name))
		return it->value;
	return mn::Str{};
}

inline static void
cli_print_help(CLI& self)
{
	mn::printerr("{}\n", self.desc);
	mn::printerr("{} command [options] INPUT...\n", self.executable_name);

	if (self.commands_schema.count > 0)
		mn::printerr("COMMANDS:\n");
	for (const auto& [command_name, command]: self.commands_schema)
	{
		mn::printerr("  {}", command.name);
		if (command.desc.count > 0)
			mn::printerr(": {}", command.desc);
		mn::printerr("\n");
	}

	mn::printerr("\n");

	if (self.options_schema.count > 0)
		mn::printerr("OPTIONS:\n");
	for (const auto& [option_name, option]: self.options_schema)
	{
		mn::printerr("  {}", option.name);
		if (option.desc.count > 0)
			mn::printerr(": {}", option.desc);
		mn::printerr("\n");
	}
}

int main(int argc, char** argv)
{
	auto compiler_folder = mn::path_absolute(mn::file_directory(argv[0], mn::memory::tmp()));
	auto std_library_folder = mn::path_join(compiler_folder, "std");
	mn_defer(mn::str_free(std_library_folder));

	auto cli = cli_new(mn::str_lit("sabrec the sabre compiler"), mn::memory::tmp());
	cli_command_add(cli, mn::str_lit("help"), mn::str_lit("prints this message"));
	cli_command_add(cli, mn::str_lit("scan"), mn::str_lit("scans the given input files and prints a token stream"));
	cli_command_add(cli, mn::str_lit("parse-expr"), mn::str_lit("scans the given input files and prints a token stream"));
	cli_command_add(cli, mn::str_lit("parse-stmt"), mn::str_lit("parses a single statement from the given files and prints it's AST representation"));
	cli_command_add(cli, mn::str_lit("parse-decl"), mn::str_lit("parses a single declaration from the given files and prints it's AST representation"));
	cli_command_add(cli, mn::str_lit("check"), mn::str_lit("performs type checking on the given files"));
	cli_command_add(cli, mn::str_lit("glsl-gen"), mn::str_lit("generates GLSL code from the given files"));
	cli_command_add(cli, mn::str_lit("reflect"), mn::str_lit("generates reflection information for the given files"));
	cli_option_add(cli, mn::str_lit("-entry"), mn::str_lit("specifies the entry point function of the given program"));

	auto err = cli_parse(cli, argc, argv);
	if (err)
	{
		mn::printerr("{}\n\n", err);
		cli_print_help(cli);
		return EXIT_FAILURE;
	}

	if (cli.cmd == "help")
	{
		cli_print_help(cli);
		return EXIT_SUCCESS;
	}
	else if (cli.cmd == "scan")
	{
		if (cli.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			cli_print_help(cli);
			return EXIT_FAILURE;
		}
		auto path = cli.input[0];

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
	else if (cli.cmd == "parse-expr")
	{
		if (cli.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			cli_print_help(cli);
			return EXIT_FAILURE;
		}
		auto path = cli.input[0];

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
	else if (cli.cmd == "parse-stmt")
	{
		if (cli.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			cli_print_help(cli);
			return EXIT_FAILURE;
		}
		auto path = cli.input[0];

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
	else if (cli.cmd == "parse-decl")
	{
		if (cli.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			cli_print_help(cli);
			return EXIT_FAILURE;
		}
		auto path = cli.input[0];

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
	else if (cli.cmd == "check")
	{
		if (cli.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			cli_print_help(cli);
			return EXIT_FAILURE;
		}
		auto path = cli.input[0];

		auto entry = cli_option_value(cli, mn::str_lit("-entry"));

		auto [answer, err] = sabre::check_file(path, mn::str_lit(""), entry, std_library_folder);
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return EXIT_SUCCESS;
	}
	else if (cli.cmd == "glsl-gen")
	{
		if (cli.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			cli_print_help(cli);
			return EXIT_FAILURE;
		}
		auto path = cli.input[0];

		auto entry = cli_option_value(cli, mn::str_lit("-entry"));

		auto [answer, err] = sabre::glsl_gen_from_file(path, mn::str_lit(""), entry, std_library_folder);
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return EXIT_SUCCESS;
	}
	else if (cli.cmd == "reflect")
	{
		if (cli.input.count != 1)
		{
			mn::printerr("no input files, you should provide path for the entry point file\n\n");
			cli_print_help(cli);
			return EXIT_FAILURE;
		}
		auto path = cli.input[0];

		auto entry = cli_option_value(cli, mn::str_lit("-entry"));

		auto [answer, err] = sabre::reflect_file(path, entry, std_library_folder);
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