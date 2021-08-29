void main_foo() {
	bool2 v2;
	bool3 v3;
	bool4 v4;
	bool v2x = v2.x;
	bool v3x = v3.x;
	bool v4x = v4.x;
	bool s = v2x && v3x && v4x;
}
void main_bar() {
	bool2 v2;
	bool3 v3;
	bool4 v4;
	bool3 v2x = v2.xxx;
	bool3 v3x = v3.xyy;
	bool3 v4x = v4.xzw;
	bool3 s = v2x || v3x && v4x;
}
bool4 main_baz() {
	bool2 v2;
	bool3 v3;
	bool4 v4;
	bool4 v2x = v2.xxxy;
	bool4 v3x = v3.xyyz;
	bool4 v4x = v4.rgba;
	return v2x || v3x || v4x;
}
