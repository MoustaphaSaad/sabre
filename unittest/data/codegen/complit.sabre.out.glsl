struct main_Point {
	vec2 p;
};
main_Point main_add(main_Point a, main_Point b) {
	vec2 _tmp_1 = vec2(a.p.x + b.p.x, a.p.y + b.p.y);
	main_Point _tmp_2 = main_Point(_tmp_1);
	return _tmp_2;
}
main_Point main_add2(main_Point a, main_Point b) {
	main_Point _tmp_3 = main_Point(a.p + b.p);
	return _tmp_3;
}
bool main_is_white(vec4 color) {
	vec4 _tmp_4 = vec4(1.0, 1.0, 1.0, 1.0);
	if (all(equal(color, _tmp_4))) {
		return true;
	} else {
		return false;
	}
}
