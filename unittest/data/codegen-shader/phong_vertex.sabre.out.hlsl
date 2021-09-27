cbuffer uniform_tmp_1: register(b0) {
	column_major float4x4 main_mvp;
};
struct main_PS_Input {
	float4 position: SV_POSITION;
	float3 vertex_position: TEXCOORD1;
	float3 vertex_normal: TEXCOORD2;
};
struct main_VS_Input {
	float3 position: TEXCOORD0;
	float3 normal: TEXCOORD1;
};
main_PS_Input main_main(main_VS_Input input) {
	float4 _tmp_2 = float4(input.position, 1.0);
	main_PS_Input _tmp_3 = {mul(main_mvp, _tmp_2), input.position, input.normal};
	return _tmp_3;
}

main_PS_Input main(main_VS_Input input)
{
	return main_main(input);
}
