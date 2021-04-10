#define DOCTEST_CONFIG_IMPLEMENT
#include <doctest/doctest.h>

int main(int argc, char** argv) {
	doctest::Context context;
	context.applyCommandLine(argc, argv);
	int res = context.run(); // run
	if (context.shouldExit()) // important - query flags (and --exit) rely on the user doing this
		return res;
	return res;
}
