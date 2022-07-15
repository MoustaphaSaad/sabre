struct main_VS_Input {
	float3 position: TEXCOORD0;
	float3 normal: TEXCOORD1;
	uint instance_id: SV_InstanceID;
};
struct main_PS_Input {
	float4 position: SV_POSITION;
	float3 vertex_position: TEXCOORD1;
	float3 vertex_normal: TEXCOORD2;
};
cbuffer uniform_tmp_1: register(b1) {
	column_major float4x4 main_mvp;
};
struct main_Instanced_Data {
	float3 position[4096];
};
cbuffer main_instanced_data: register(b0) {
	float3 main_instanced_data_position[4096]: packoffset(c0);
};
main_PS_Input main_main(main_VS_Input input) {
	float4 _tmp_2 = float4(input.position + main_instanced_data_position[input.instance_id], 1.0);
	main_PS_Input _tmp_3 = {mul(main_mvp, _tmp_2), input.position, input.normal};
	return _tmp_3;
}

main_PS_Input main(main_VS_Input input)
{
	return main_main(input);
}
