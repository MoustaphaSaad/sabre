#version 450
layout(location = 0) in vec3 vs_input_position;

struct main_VS_Input {
	vec3 position;
};
struct main_PS_Input {
	vec4 position;
};
main_PS_Input main_main(main_VS_Input vs_input) {
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
