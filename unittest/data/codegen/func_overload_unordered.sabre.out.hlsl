float2 main_add(float2 a, float2 b) {
	return a + b;
}
float2 main_add(float3 a, float3 b) {
	return main_add(a.xy, b.yx);
}
float2 main_koko(float4 a, float4 b) {
	return main_add(a.xyz, b.zyx);
}
