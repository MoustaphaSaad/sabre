void main_foo() {
	bvec2 v2;
	bvec3 v3;
	bvec4 v4;
	bool v2x = v2.x;
	bool v3x = v3.x;
	bool v4x = v4.x;
	bool s = v2x && v3x && v4x;
}
void main_bar() {
	bvec2 v2;
	bvec3 v3;
	bvec4 v4;
	bvec3 v2x = v2.xxx;
	bvec3 v3x = v3.xyy;
	bvec3 v4x = v4.xzw;
	bvec3 s = v2x || v3x && v4x;
}
bvec4 main_baz() {
	bvec2 v2;
	bvec3 v3;
	bvec4 v4;
	bvec4 v2x = v2.xxxy;
	bvec4 v3x = v3.xyyz;
	bvec4 v4x = v4.rgba;
	return v2x || v3x || v4x;
}