struct main_Foo_int_float {
	int x;
	int y;
	float z;
	float w;
};
struct main_Bar_int {
	main_Foo_int_float foo;
};
main_Foo_int_float _tmp_1 = {1, 0, 1.5, 0.0};
main_Bar_int _tmp_2 = {_tmp_1};
main_Bar_int main_v = _tmp_2;
main_Foo_int_float _tmp_3 = {1, 0, 1.5, 0.0};
main_Bar_int _tmp_4 = {_tmp_3};
main_Bar_int main_v2 = _tmp_4;
struct main_Foo_uint_float {
	uint x;
	uint y;
	float z;
	float w;
};
struct main_Bar_uint {
	main_Foo_uint_float foo;
};
main_Foo_uint_float _tmp_5 = {uint(1), 0, 1.5, 0.0};
main_Bar_uint _tmp_6 = {_tmp_5};
main_Bar_uint main_v3 = _tmp_6;
