#version 460

layout (location = 0) out vec4 fragColor;

void main()
{
    const int _tmp_1[7] = int[7](1, 2, 3, 4, 5, 0, 0);
    const int _tmp_2[1][7] = int[1][7](_tmp_1);
    const int main_main_numbers[1][7] = _tmp_2;
    const int main_main_xo[7] = main_main_numbers[0];
    const int main_main_my_number = main_main_numbers[0][3];
    int xo2[7] = main_main_xo;
	fragColor = vec4(0.4, 0.4, 0.8, 1.0);
}