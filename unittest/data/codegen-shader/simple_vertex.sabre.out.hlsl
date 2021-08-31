struct main_VS_Input {
	float3 position: TEXCOORD0;
};
struct main_PS_Input {
	float4 position: SV_POSITION;
};
struct main_Config {
	float3 clip_pos;
	int clip_enabled;
	float3 clip_norm;
	float window_level;
	float3 roi_min;
	int crop_enabled;
	float3 roi_max;
	float window_width;
	float3 slicing_dir;
	float2 noise_tex_size;
};
cbuffer main_config: register(b2) {
	float3 main_config_clip_pos: packoffset(c0);
	int main_config_clip_enabled: packoffset(c0.w);
	float3 main_config_clip_norm: packoffset(c1);
	float main_config_window_level: packoffset(c1.w);
	float3 main_config_roi_min: packoffset(c2);
	int main_config_crop_enabled: packoffset(c2.w);
	float3 main_config_roi_max: packoffset(c3);
	float main_config_window_width: packoffset(c3.w);
	float3 main_config_slicing_dir: packoffset(c4);
	float2 main_config_noise_tex_size: packoffset(c5);
};
main_PS_Input main_main(main_VS_Input vs_input) {
	main_config_clip_pos;
	float4 _tmp_1 = float4(vs_input.position, 1);
	main_PS_Input _tmp_2 = {_tmp_1};
	return _tmp_2;
}

main_PS_Input main(main_VS_Input vs_input)
{
	return main_main(vs_input);
}
