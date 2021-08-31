struct main_PS_Input {
	float4 position;
	float2 texcoord;
	float4 color;
};
struct main_Bar {
	float x;
};
struct main_Foo {
	main_Bar bar;
};
cbuffer main_foo: register(b1) {
	main_Bar main_foo_bar: packoffset(c0);
};
Texture2D<float4> main_texture: register(t0);
float4 main_main(main_PS_Input input) {
	return input.color;
}
