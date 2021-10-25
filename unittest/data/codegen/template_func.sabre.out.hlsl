struct main_Point_int {
	int x;
	int y;
};
main_Point_int main_add_int(main_Point_int a, main_Point_int b) {
	main_Point_int _tmp_1 = {a.x + b.x, a.y + b.y};
	return _tmp_1;
}
main_Point_int main_add_usage(main_Point_int a, main_Point_int b) {
	return main_add_int(a, b);
}
