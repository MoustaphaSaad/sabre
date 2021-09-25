#include <doctest/doctest.h>

#include <sabre/Utils.h>

#include <mn/Path.h>
#include <mn/IO.h>
#include <mn/Defer.h>
#include <mn/Log.h>

inline static mn::Str
load_out_data(const mn::Str& filepath)
{
	auto out_path = mn::str_tmpf("{}.out", filepath);
	CHECK(mn::path_is_file(out_path));
	return mn::file_content_str(out_path, mn::memory::tmp());
}

inline static mn::Str
load_out_glsl_data(const mn::Str& filepath)
{
	auto out_path = mn::str_tmpf("{}.out.glsl", filepath);
	CHECK(mn::path_is_file(out_path));
	return mn::file_content_str(out_path, mn::memory::tmp());
}

inline static mn::Str
load_out_hlsl_data(const mn::Str& filepath)
{
	auto out_path = mn::str_tmpf("{}.out.hlsl", filepath);
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
		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::scan_file(filepath, f.name);
		CHECK(err == false);
		mn_defer(mn::str_free(answer));

		if (answer != out_data)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(answer == out_data);
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
		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::parse_expr_from_file(filepath, f.name);
		CHECK(err == false);
		mn_defer(mn::str_free(answer));

		auto match = answer == out_data;
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(match == true);
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
		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::parse_stmt_from_file(filepath, f.name);
		CHECK(err == false);
		mn_defer(mn::str_free(answer));

		auto match = answer == out_data;
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(match == true);
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
		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::parse_decl_from_file(filepath, f.name);
		CHECK(err == false);
		mn_defer(mn::str_free(answer));

		auto match = answer == out_data;
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(match == true);
	}
}

TEST_CASE("[sabre]: typecheck")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "check");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_suffix(f.name, ".out"))
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);
		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::check_file(filepath, f.name, mn::str_lit(""), {});
		CHECK(err == false);
		mn_defer(mn::str_free(answer));
		mn::str_replace(answer, "\r\n", "\n");
		mn::str_trim(answer);

		auto match = answer == out_data;
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(match == true);
	}
}


TEST_CASE("[sabre]: glsl")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "codegen");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_find_last(f.name, ".out", f.name.count) != SIZE_MAX)
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);

		if (mn::path_is_file(mn::str_tmpf("{}.out.glsl", filepath)) == false)
		{
			mn::log_warning("missing glsl output for '{}'", filepath);
			continue;
		}

		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_glsl_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::glsl_gen_from_file(filepath, f.name, mn::str_lit(""), {});
		CHECK(err == false);
		mn_defer(mn::str_free(answer));
		mn::str_replace(answer, "\r\n", "\n");
		mn::str_trim(answer);

		auto match = answer == out_data;
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(match == true);
	}
}

TEST_CASE("[sabre]: hlsl")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "codegen");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_find_last(f.name, ".out", f.name.count) != SIZE_MAX)
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);

		if (mn::path_is_file(mn::str_tmpf("{}.out.hlsl", filepath)) == false)
		{
			mn::log_warning("missing hlsl output for '{}'", filepath);
			continue;
		}

		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_hlsl_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::hlsl_gen_from_file(filepath, f.name, mn::str_lit(""), {});
		CHECK(err == false);
		mn_defer(mn::str_free(answer));
		mn::str_replace(answer, "\r\n", "\n");
		mn::str_trim(answer);

		auto match = answer == out_data;
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(match == true);
	}
}

TEST_CASE("[sabre]: glsl-shader")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "codegen-shader");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_find_last(f.name, ".out", f.name.count) != SIZE_MAX)
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);

		if (mn::path_is_file(mn::str_tmpf("{}.out.glsl", filepath)) == false)
		{
			mn::log_warning("missing glsl output for '{}'", filepath);
			continue;
		}

		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_glsl_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::glsl_gen_from_file(filepath, f.name, mn::str_lit("main"), {});
		CHECK(err == false);
		mn_defer(mn::str_free(answer));
		mn::str_replace(answer, "\r\n", "\n");
		mn::str_trim(answer);

		auto match = answer == out_data;
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(match == true);
	}
}

TEST_CASE("[sabre]: hlsl-shader")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "codegen-shader");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_find_last(f.name, ".out", f.name.count) != SIZE_MAX)
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);

		if (mn::path_is_file(mn::str_tmpf("{}.out.hlsl", filepath)) == false)
		{
			mn::log_warning("missing hlsl output for '{}'", filepath);
			continue;
		}

		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_hlsl_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::hlsl_gen_from_file(filepath, f.name, mn::str_lit("main"), {});
		CHECK(err == false);
		mn_defer(mn::str_free(answer));
		mn::str_replace(answer, "\r\n", "\n");
		mn::str_trim(answer);

		auto match = answer == out_data;
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(match == true);
	}
}

TEST_CASE("[sabre]: reflect")
{
	mn_defer(mn::memory::tmp()->clear_all());

	auto base_dir = mn::path_join(mn::str_tmp(), DATA_DIR, "reflect");
	auto files = mn::path_entries(base_dir, mn::memory::tmp());
	for (auto f: files)
	{
		if (f.kind != mn::Path_Entry::KIND_FILE || f.name == "." || f.name == "..")
			continue;

		if (mn::str_suffix(f.name, ".out"))
			continue;

		auto filepath = mn::path_join(mn::str_tmp(), base_dir, f.name);
		mn::log_info("testing file: {}...", filepath);
		auto out_data = load_out_data(filepath);
		mn::str_replace(out_data, "\r\n", "\n");
		mn::str_trim(out_data);

		auto [answer, err] = sabre::reflect_file(filepath, mn::str_lit("main"), {});
		CHECK(err == false);
		mn_defer(mn::str_free(answer));
		mn::str_replace(answer, "\r\n", "\n");
		mn::str_trim(answer);

		auto match = answer == out_data;
		if (match == false)
		{
			mn::print("expected:\n{}\n", out_data);
			mn::print("answer:\n{}\n", answer);
		}
		REQUIRE(match == true);
	}
}