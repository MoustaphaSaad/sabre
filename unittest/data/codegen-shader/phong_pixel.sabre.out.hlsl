struct main_PS_Input {
	float4 position: SV_POSITION;
	float3 vertex_position: TEXCOORD1;
	float3 vertex_normal: TEXCOORD2;
	uint primitive_id: SV_PrimitiveID;
};
struct main_PS_Output {
	float4 color: SV_TARGET0;
};
struct main_Model {
	column_major float4x4 model_matrix;
	column_major float4x4 model_inverse_transposed;
	float4 color;
};
cbuffer main_model: register(b3) {
	column_major float4x4 main_model_model_matrix: packoffset(c0);
	column_major float4x4 main_model_model_inverse_transposed: packoffset(c4);
	float4 main_model_color: packoffset(c8);
};
struct main_Light {
	float3 direction;
	float4 color;
};
cbuffer main_light: register(b2) {
	float3 main_light_direction: packoffset(c0);
	float4 main_light_color: packoffset(c1);
};
main_PS_Output main_main(main_PS_Input input) {
	float4 _tmp_1 = float4(input.vertex_position, 1.0);
	float4 world_pos = mul(main_model_model_matrix, _tmp_1);
	float4 _tmp_2 = float4(input.vertex_normal, 0.0);
	float3 normal = (mul(main_model_model_inverse_transposed, _tmp_2)).xyz;
	normal = normalize(normal);
	static const float main_main_ambient_factor = 0.3;
	float4 ambient_color = main_main_ambient_factor * main_light_color;
	float3 dir = normalize(-main_light_direction);
	float diffuse_factor = max(dot(normal, dir), 0.0);
	float4 diffuse_color = diffuse_factor * main_light_color;
	main_PS_Output output;
	output.color.rgb = (ambient_color.rgb + diffuse_color.rgb) * main_model_color.rgb;
	output.color.a = main_model_color.a;
	return output;
}

main_PS_Output main(main_PS_Input input)
{
	return main_main(input);
}
