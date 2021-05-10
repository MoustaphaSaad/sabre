#include <mn/IO.h>
#include <mn/Defer.h>

#include <sabre/Utils.h>

int main(int argc, char** argv)
{
	if (argc < 3)
	{
		mn::printerr("invalid command line args\n");
		return EXIT_FAILURE;
	}

	auto cmd = mn::str_lit(argv[1]);
	auto path = mn::str_lit(argv[2]);
	if (cmd == "scan")
	{
		auto [answer, err] = sabre::scan_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return 0;
	}
	else if (cmd == "parse-expr")
	{
		auto [answer, err] = sabre::parse_expr_from_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return 0;
	}
	else if (cmd == "parse-stmt")
	{
		auto [answer, err] = sabre::parse_stmt_from_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return 0;
	}
	else if (cmd == "parse-decl")
	{
		auto [answer, err] = sabre::parse_decl_from_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return 0;
	}
	else if (cmd == "check")
	{
		auto [answer, err] = sabre::check_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return 0;
	}
	else if (cmd == "glsl-gen")
	{
		auto [answer, err] = sabre::glsl_gen_from_file(path, mn::str_lit(""));
		if (err)
		{
			mn::printerr("{}\n", err);
			return EXIT_FAILURE;
		}
		mn_defer(mn::str_free(answer));
		mn::print("{}\n", answer);
		return 0;
	}
	else
	{
		mn::printerr("invalid command line args\n");
		return EXIT_FAILURE;
	}
}