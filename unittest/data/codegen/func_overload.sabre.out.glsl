vec3 main_add(vec3 a, vec3 b) {
	return a + b;
}
vec3 main_koko(vec4 a, vec4 b) {
	return main_add(a.xyz, b.zyx);
}