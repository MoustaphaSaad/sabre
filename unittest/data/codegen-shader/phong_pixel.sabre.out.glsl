#version 450
layout(location = 0) in vec3 RESERVED_input_vertex_position;
layout(location = 1) in vec3 RESERVED_input_vertex_normal;

layout(location = 0) out vec4 _entry_point_output_color;

layout(binding = 2, std140) uniform main_light {
	vec3 main_light_direction;
	vec4 main_light_color;
};
layout(binding = 3, std140) uniform main_model {
	mat4 main_model_model_matrix;
	mat4 main_model_model_inverse_transposed;
	vec4 main_model_color;
};
struct main_PS_Output {
	vec4 color;
};
struct main_Light {
	vec3 direction;
	vec4 color;
};
struct main_Model {
	mat4 model_matrix;
	mat4 model_inverse_transposed;
	vec4 color;
};
struct main_PS_Input {
	vec4 position;
	vec3 vertex_position;
	vec3 vertex_normal;
};
main_PS_Output main_main(main_PS_Input RESERVED_input) {
	vec4 _tmp_1 = vec4(RESERVED_input.vertex_position, 1.0);
	vec4 world_pos = main_model_model_matrix * _tmp_1;
	vec4 _tmp_2 = vec4(RESERVED_input.vertex_normal, 0.0);
	vec3 normal = (main_model_model_inverse_transposed * _tmp_2).xyz;
	normal = normalize(normal);
	const float main_main_ambient_factor = 0.3;
	vec4 ambient_color = main_main_ambient_factor * main_light_color;
	vec3 dir = normalize(-main_light_direction);
	float diffuse_factor = max(dot(normal, dir), 0.0);
	vec4 diffuse_color = diffuse_factor * main_light_color;
	main_PS_Output RESERVED_output;
	RESERVED_output.color.rgb = (ambient_color.rgb + diffuse_color.rgb) * main_model_color.rgb;
	RESERVED_output.color.a = main_model_color.a;
	return RESERVED_output;
}

void main() {
	main_PS_Input RESERVED_input;
	RESERVED_input.position = gl_FragCoord;
	RESERVED_input.vertex_position = RESERVED_input_vertex_position;
	RESERVED_input.vertex_normal = RESERVED_input_vertex_normal;
	
	main_PS_Output _tmp_3 = main_main(RESERVED_input);
	_entry_point_output_color = _tmp_3.color;
}
