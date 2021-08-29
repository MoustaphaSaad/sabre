struct main_PS_Input {
	vec4 position;
	vec2 texcoord;
	vec4 color;
};
struct main_Bar {
	float x;
};
struct main_Foo {
	main_Bar bar;
};
layout(binding = 1, std140) uniform main_foo {
	main_Bar main_foo_bar;
};
layout(binding = 0) uniform sampler2D main_texture;
vec4 main_main(main_PS_Input RESERVED_input) {
	return RESERVED_input.color;
}