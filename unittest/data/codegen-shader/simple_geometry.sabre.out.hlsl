struct points_GS_Input {
	float3 center: TEXCOORD0;
	float4 v_color: TEXCOORD1;
	float v_radius: TEXCOORD2;
};
struct points_PS_Input {
	float4 position: SV_POSITION;
	float4 v_color: TEXCOORD1;
	float v_radius: TEXCOORD2;
	float2 v_center: TEXCOORD3;
	float2 pos_viewport: TEXCOORD4;
};
struct points_Camera {
	column_major float4x4 viewproj;
	float3 right;
	float3 up;
	float2 viewport;
};
cbuffer points_camera: register(b0) {
	column_major float4x4 points_camera_viewproj: packoffset(c0);
	float3 points_camera_right: packoffset(c4);
	float3 points_camera_up: packoffset(c5);
	float2 points_camera_viewport: packoffset(c6);
};
void points_foo(points_PS_Input a, points_PS_Input b, points_PS_Input c, inout TriangleStream<points_PS_Input> _tmp_1) {
	_tmp_1.Append(a);
	_tmp_1.Append(b);
	_tmp_1.Append(c);
}
void points_main(points_GS_Input input[1], inout TriangleStream<points_PS_Input> _tmp_1) {
	float3 a = input[0].center + input[0].v_radius * points_camera_right + input[0].v_radius * points_camera_up;
	float3 b = input[0].center + input[0].v_radius * points_camera_right - input[0].v_radius * points_camera_up;
	float3 c = input[0].center - input[0].v_radius * points_camera_right + input[0].v_radius * points_camera_up;
	float3 d = input[0].center - input[0].v_radius * points_camera_right - input[0].v_radius * points_camera_up;
	points_PS_Input v0;
	points_PS_Input v1;
	points_PS_Input v2;
	points_PS_Input v3;
	float4 _tmp_2 = float4(a, 1);
	v0.position = mul(points_camera_viewproj, _tmp_2);
	float4 _tmp_3 = float4(b, 1);
	v1.position = mul(points_camera_viewproj, _tmp_3);
	float4 _tmp_4 = float4(c, 1);
	v2.position = mul(points_camera_viewproj, _tmp_4);
	float4 _tmp_5 = float4(d, 1);
	v3.position = mul(points_camera_viewproj, _tmp_5);
	v0.v_color = input[0].v_color;
	v1.v_color = input[0].v_color;
	v2.v_color = input[0].v_color;
	v3.v_color = input[0].v_color;
	v0.v_radius = input[0].v_radius;
	v1.v_radius = input[0].v_radius;
	v2.v_radius = input[0].v_radius;
	v3.v_radius = input[0].v_radius;
	float4 _tmp_6 = float4(input[0].center, 1);
	float4 pos = mul(points_camera_viewproj, _tmp_6);
	float2 _tmp_7 = float2(1, 1);
	v0.v_center = 0.5 * (pos.xy + _tmp_7) * points_camera_viewport;
	v1.v_center = v0.v_center;
	v2.v_center = v0.v_center;
	v3.v_center = v0.v_center;
	float2 _tmp_8 = float2(1, 1);
	v0.pos_viewport = 0.5 * (v0.position.xy + _tmp_8) * points_camera_viewport;
	float2 _tmp_9 = float2(1, 1);
	v1.pos_viewport = 0.5 * (v1.position.xy + _tmp_9) * points_camera_viewport;
	float2 _tmp_10 = float2(1, 1);
	v2.pos_viewport = 0.5 * (v2.position.xy + _tmp_10) * points_camera_viewport;
	float2 _tmp_11 = float2(1, 1);
	v3.pos_viewport = 0.5 * (v3.position.xy + _tmp_11) * points_camera_viewport;
	_tmp_1.Append(v0);
	_tmp_1.Append(v3);
	_tmp_1.Append(v1);
	_tmp_1.Append(v0);
	_tmp_1.Append(v2);
	_tmp_1.Append(v3);
	points_foo(v0, v3, v1, _tmp_1);
	_tmp_1.RestartStrip();
}

[maxvertexcount(6)] void main(point points_GS_Input input[1], inout TriangleStream<points_PS_Input> _tmp_1)
{
	points_main(input, _tmp_1);
}
