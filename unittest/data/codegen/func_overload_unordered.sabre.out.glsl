vec2 main_add(vec2 a, vec2 b) {
	return a + b;
}
vec2 main_add(vec3 a, vec3 b) {
	return main_add(a.xy, b.yx);
}
vec2 main_koko(vec4 a, vec4 b) {
	return main_add(a.xyz, b.zyx);
}