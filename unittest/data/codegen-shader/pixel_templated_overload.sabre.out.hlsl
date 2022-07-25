struct main_Input {
};
Texture3D<float4> B_texture_3d;
float4 A_foo_float4(Texture3D<float4> _tmp_1) {
	float4 _tmp_2 = float4(0.0, 0.0, 0.0, 0.0);
	return _tmp_2;
}
Texture2D<float4> B_texture_2d;
float4 A_foo_float4(Texture2D<float4> _tmp_3) {
	float4 _tmp_4 = float4(0.0, 0.0, 0.0, 0.0);
	return _tmp_4;
}
void B_test() {
	A_foo(B_texture_3d);
	A_foo(B_texture_2d);
}
float4 main_main(main_Input input) {
	B_test();
	float4 _tmp_5 = float4(0.0, 0.0, 0.0, 0.0);
	return _tmp_5;
}

float4 main(main_Input input)
{
	return main_main(input);
}
