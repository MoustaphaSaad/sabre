#version 450
layout(location = 0) in vec3 RESERVED_input_position;
layout(location = 1) in vec3 RESERVED_input_normal;

layout(location = 0) out vec3 _entry_point_output_vertex_position;
layout(location = 1) out vec3 _entry_point_output_vertex_normal;

struct main_VS_Input {
	vec3 position;
	vec3 normal;
};
struct main_PS_Input {
	vec4 position;
	vec3 vertex_position;
	vec3 vertex_normal;
};
layout(binding = 0, std140) uniform uniform_tmp_1 {
	mat4 main_mvp;
};
main_PS_Input main_main(main_VS_Input RESERVED_input) {
	vec4 _tmp_2 = vec4(RESERVED_input.position, 1.0);
	main_PS_Input _tmp_3 = main_PS_Input(main_mvp * _tmp_2, RESERVED_input.position, RESERVED_input.normal);
	return _tmp_3;
}

void main() {
	main_VS_Input RESERVED_input;
	RESERVED_input.position = RESERVED_input_position;
	RESERVED_input.normal = RESERVED_input_normal;
	
	main_PS_Input _tmp_4 = main_main(RESERVED_input);
	gl_Position = _tmp_4.position;
	_entry_point_output_vertex_position = _tmp_4.vertex_position;
	_entry_point_output_vertex_normal = _tmp_4.vertex_normal;
}
