#version 300 es

precision highp float;
in vec4 position;

uniform mat4 camera;
uniform mat4 projection;

out vec4 fPosition;

void main()
{
    fPosition = position;
    gl_Position = projection * camera * fPosition;
}
