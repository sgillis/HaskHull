#version 300 es

precision highp float;
in vec4 position;
in vec4 normal;
in vec2 texCoords;

uniform mat4 camera;
uniform mat4 projection;
uniform mat4 model;

out vec4 fNormal;
out vec4 fPosition;
out vec2 fCoords;

void main()
{
    fPosition = model * position;
    gl_Position = projection * camera * fPosition;
    fNormal = normal;
    fCoords = texCoords;
}
