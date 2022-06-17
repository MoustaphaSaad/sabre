static const int main_IMAGE_SIZE = 16;
RWTexture2D<float4> foo_image;
void main_main(uint3 global_index, uint3 group_index) {
	float4 _tmp_1 = float4(float2(group_index.xy) / main_IMAGE_SIZE, 1, 1);
	foo_image[global_index.xy] = _tmp_1;
}

[numthreads(16, 16, 1)]
void main(uint3 global_index: SV_DispatchThreadID, uint3 group_index: SV_GroupThreadID)
{
	main_main(global_index, group_index);
}
