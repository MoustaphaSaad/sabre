struct main_Numbers {
	float4 op1;
	float4 op2;
	int op3;
	int op4;
	int values[10];
};
ByteAddressBuffer main_numbers;
void main_main() {
	float _tmp_1 = asfloat(main_numbers.Load(0));
	float _tmp_2 = asfloat(main_numbers.Load(24));
	float res = _tmp_1 + _tmp_2;
	float4 _tmp_3 = (main_numbers.Load4(0));
	float4 _tmp_4 = (main_numbers.Load4(16));
	float2 res2 = _tmp_3.yx + _tmp_4.wz;
	int _tmp_5 = int(main_numbers.Load(32));
	int _tmp_6 = int(main_numbers.Load(36));
	int res3 = _tmp_5 + _tmp_6;
	int _tmp_7 = int(main_numbers.Load(32));
	int _tmp_8 = int(main_numbers.Load(36));
	int _tmp_9 = int(main_numbers.Load(40 + ((_tmp_7 + _tmp_8) * 4)));
	int koko = _tmp_9;
}

[numthreads(1, 1, 1)]
void main()
{
	main_main();
}
