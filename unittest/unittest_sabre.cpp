#include <doctest/doctest.h>

#include <sabre/Utils.h>

#include <mn/Path.h>
#include <mn/IO.h>
#include <mn/Defer.h>

inline static mn::Str
load_out_data(const mn::Str& filepath)
{
	auto out_path = mn::str_tmpf("{}.out", filepath);
	CHECK(mn::path_is_file(out_path));
	return mn::file_content_str(out_path, mn::memory::tmp());
}

TEST_CASE("[sabre]: lex")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "lex");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_suffix(f.name, ".out"))
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::scan_file(filepath, f.name);
		CHECK(err == false);
		mn_defer(mn::str_free(answer));

		CHECK(answer == out_data);
		if (answer != out_data)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
			CHECK(false);
		}
	}
}

TEST_CASE("[sabre]: parse-expr")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "expr");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_suffix(f.name, ".out"))
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::parse_expr_from_file(filepath, f.name);
		CHECK(err == false);
		mn_defer(mn::str_free(answer));

		auto match = answer == out_data;
		CHECK(match == true);
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
	}
}

TEST_CASE("[sabre]: parse-stmt")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "stmt");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_suffix(f.name, ".out"))
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::parse_stmt_from_file(filepath, f.name);
		CHECK(err == false);
		mn_defer(mn::str_free(answer));

		auto match = answer == out_data;
		CHECK(match == true);
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
	}
}

TEST_CASE("[sabre]: parse-decl")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "decl");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_suffix(f.name, ".out"))
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::parse_decl_from_file(filepath, f.name);
		CHECK(err == false);
		mn_defer(mn::str_free(answer));

		auto match = answer == out_data;
		CHECK(match == true);
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
	}
}