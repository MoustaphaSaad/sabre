struct main_VS_Input {
	float3 position: TEXCOORD0;
};
struct main_PS_Input {
	float4 position: SV_POSITION;
};
main_PS_Input main_main(main_VS_Input vs_input) {
	float4 _tmp_1 = float4(vs_input.position, 1);
	main_PS_Input _tmp_2 = {_tmp_1};
	return _tmp_2;
}

main_PS_Input main(main_VS_Input vs_input)
{
	return main_main(vs_input);
}
