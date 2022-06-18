struct main_Numbers {
	float4 op1;
	float4 op2;
	int op3;
	int op4;
	int values[10];
};
RWByteAddressBuffer main_numbers: register(u0);
void main_main() {
	int _tmp_1 = int(main_numbers.Load(32));
	int _tmp_2 = int(main_numbers.Load(36));
	int _tmp_3 = int(main_numbers.Load(40 + ((_tmp_1 + _tmp_2) * 4)));
	main_numbers.Store(40 + ((9) * 4), uint(_tmp_3));
	int _tmp_4 = int(main_numbers.Load(40 + ((9) * 4)));
	main_numbers.Store(40 + ((9) * 4), uint(_tmp_4 + 42));
	int _tmp_5 = int(main_numbers.Load(40 + ((9) * 4)));
	main_numbers.Store(40 + ((9) * 4), uint(_tmp_5 - 1));
}

[numthreads(1, 1, 1)]
void main()
{
	main_main();
}
