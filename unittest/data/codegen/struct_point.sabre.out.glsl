struct main_Point {
	vec2 p;
};
main_Point main_add(main_Point a, main_Point b) {
	main_Point res;
	res.p = a.p + b.p;
	return res;
}