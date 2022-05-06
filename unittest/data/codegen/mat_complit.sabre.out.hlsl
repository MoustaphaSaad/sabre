void main_koko() {
	float3 a;
	float3 b;
	float3 c;
	column_major float3x3 _tmp_1 = {a, b, c};
	column_major float3x3 tbn = _tmp_1;
	tbn[0] = b;
}
