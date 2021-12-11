int main_convert_bool_to_int(bool x) {
	if (x) {
		return 0;
	} else {
		return 1;
	}
}
int main_sum_to_n(bool x) {
	int n = main_convert_bool_to_int(x);
	int res = 0;
	{ // for scope
		// for init statement
		int i = 0;
		while (i < n) {
			// for body
			if (i % 2 == 0) {
				++i;
				continue;
			} else if (i % 3 == 0) {
				break;
			} else if (i % 5 == 0) {
				discard;
			}
			res += i;
			// for post statement
			++i;
		}
	} // for scope
	return res;
}
