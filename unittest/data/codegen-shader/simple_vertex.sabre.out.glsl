#version 450
layout(location = 0) in vec3 vs_input_position;

struct main_Config {
	vec3 clip_pos;
	int clip_enabled;
	vec3 clip_norm;
	float window_level;
	vec3 roi_min;
	int crop_enabled;
	vec3 roi_max;
	float window_width;
	vec3 slicing_dir;
	vec2 noise_tex_size;
};
layout(binding = 2, std140) uniform main_config {
	vec3 main_config_clip_pos;
	int main_config_clip_enabled;
	vec3 main_config_clip_norm;
	float main_config_window_level;
	vec3 main_config_roi_min;
	int main_config_crop_enabled;
	vec3 main_config_roi_max;
	float main_config_window_width;
	vec3 main_config_slicing_dir;
	vec2 main_config_noise_tex_size;
};
struct main_PS_Input {
	vec4 position;
};
struct main_VS_Input {
	vec3 position;
};
main_PS_Input main_main(main_VS_Input vs_input) {
	main_config_clip_pos;
	vec4 _tmp_1 = vec4(vs_input.position, 1);
	main_PS_Input _tmp_2 = main_PS_Input(_tmp_1);
	return _tmp_2;
}

void main() {
	main_VS_Input vs_input;
	vs_input.position = vs_input_position;
	
	main_PS_Input _tmp_3 = main_main(vs_input);
	gl_Position = _tmp_3.position;
}
