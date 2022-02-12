void main_foo() {
	{ // for scope
		// for init statement
		int i = 0;
		[loop]
		while (i < 10) {
			// for body
			if (i == 0) {
				++i;
				continue;
			}
			// for post statement
			++i;
		}
	} // for scope
}
