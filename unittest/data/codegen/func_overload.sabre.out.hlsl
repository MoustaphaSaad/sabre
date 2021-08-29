float3 main_add(float3 a, float3 b) {
	return a + b;
}
float3 main_koko(float4 a, float4 b) {
	return main_add(a.xyz, b.zyx);
}
