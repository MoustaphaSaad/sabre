struct main_koko {
	float3 x;
	float3 y;
};
void main_main() {
	const int _tmp_1[5] = {1, 2, 3, 4, 5};
	const int _tmp_2[1][5] = {_tmp_1};
	const int main_main_numbers[1][5] = _tmp_2;
	const int main_main_xo[5] = main_main_numbers[0];
	const int main_main_my_number = main_main_numbers[0][1];
	int xo2[5] = main_main_xo;
	float3 _tmp_3 = float3(1, 2, 3);
	float3 RESERVED_point = _tmp_3;
	float3 _tmp_4 = float3(0.0, 0.0, 3);
	float3 _tmp_5 = float3(4, 5, 6);
	main_koko _tmp_6 = {_tmp_4, _tmp_5};
	main_koko _tmp_7[1] = {_tmp_6};
	main_koko kokos[1] = _tmp_7;
	float3 _tmp_8 = float3(7, 8, 9);
	main_koko _tmp_9 = {float3(0.0, 0.0, 0.0), _tmp_8};
	main_koko k = _tmp_9;
}
main_koko main_get_koko() {
	float3 _tmp_10 = float3(1, 0.0, 0.0);
	float3 _tmp_11 = float3(0, 1, 2);
	main_koko _tmp_12 = {_tmp_10, _tmp_11};
	return _tmp_12;
}
