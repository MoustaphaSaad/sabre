#version 460

layout (location = 0) out vec4 fragColor;

struct Dir_Light
{
    vec3 color, dir;
};

struct Ambient_Light
{
    vec3 color;
};

layout(std140, binding=0) uniform Lighting {
    int dir_lights_count;
    int ambient_lights_count;
    Dir_Light dir_lights[4];
    Ambient_Light ambient_lights[1];
};

void main()
{
	fragColor = vec4(0.4, 0.4, 0.8, 1.0);
}