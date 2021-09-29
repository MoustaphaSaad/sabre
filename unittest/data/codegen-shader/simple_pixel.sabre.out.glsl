#version 450
layout(location = 0) out vec4 _entry_point_output_color;

struct main_PS_Input {
	vec4 position;
};
struct main_PS_Output {
	vec4 color;
	float depth;
};
main_PS_Output main_main(main_PS_Input RESERVED_input) {
	main_PS_Output _tmp_1 = main_PS_Output(RESERVED_input.position, RESERVED_input.position.z);
	return _tmp_1;
}

void main() {
	main_PS_Input RESERVED_input;
	RESERVED_input.position = gl_FragCoord;
	
	main_PS_Output _tmp_2 = main_main(RESERVED_input);
	_entry_point_output_color = _tmp_2.color;
	gl_FragDepth = _tmp_2.depth;
}
