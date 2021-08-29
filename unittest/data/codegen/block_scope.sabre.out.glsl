float main_foo(float d) {
	if (d > 1) {
		float d_inv = 1 - d;
		float d_inv2 = d - 1;
		return d_inv + d_inv2;
	}
	return d;
}
