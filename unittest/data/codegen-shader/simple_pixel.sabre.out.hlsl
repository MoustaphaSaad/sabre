struct main_PS_Output {
	float4 color: SV_TARGET0;
	float depth: SV_DEPTH;
};
struct main_PS_Input {
	float4 position: SV_POSITION;
};
main_PS_Output main_main(main_PS_Input input) {
	main_PS_Output _tmp_1 = {input.position, input.position.z};
	return _tmp_1;
}

main_PS_Output main(main_PS_Input input)
{
	return main_main(input);
}
