RWTexture2D<float4> main_image;
void main_main(uint3 index) {
	float4 _tmp_1 = float4(float2(index.xy) / 16, 1, 1);
	main_image[index.xy] = _tmp_1;
}

[numthreads(16, 16, 1)]
void main(uint3 index: SV_DispatchThreadID)
{
	main_main(index);
}
