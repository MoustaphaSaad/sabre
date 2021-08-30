#define main_Dir int
#define main_Dir_Up 1
#define main_Dir_Down 2
#define main_Dir_Left 4
#define main_Dir_Right 8
#define main_Dir_Down_Left 6
#define main_Dir_Up_Right 9
const main_Dir main_koko = main_Dir_Left;
float2 main_offset(main_Dir d) {
	if (d == main_Dir_Up) {
		float2 _tmp_1 = float2(0, -1);
		return _tmp_1;
	} else if (d == main_Dir_Down) {
		float2 _tmp_2 = float2(0, 1);
		return _tmp_2;
	} else if (d == main_Dir_Left) {
		float2 _tmp_3 = float2(-1, 0);
		return _tmp_3;
	} else if (d == main_Dir_Right) {
		float2 _tmp_4 = float2(1, 0);
		return _tmp_4;
	} else {
		float2 _tmp_5 = float2(0.0, 0.0);
		return _tmp_5;
	}
}
