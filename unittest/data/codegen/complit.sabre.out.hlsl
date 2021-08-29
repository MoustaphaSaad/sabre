struct main_Point {
	float2 p;
};
main_Point main_add(main_Point a, main_Point b) {
	float2 _tmp_1 = float2(a.p.x + b.p.x, a.p.y + b.p.y);
	main_Point _tmp_2 = {_tmp_1};
	return _tmp_2;
}
main_Point main_add2(main_Point a, main_Point b) {
	main_Point _tmp_3 = {a.p + b.p};
	return _tmp_3;
}
bool main_is_white(float4 color) {
	float4 _tmp_4 = float4(1.0, 1.0, 1.0, 1.0);
	if (all(color == _tmp_4)) {
		return true;
	} else {
		return false;
	}
}
