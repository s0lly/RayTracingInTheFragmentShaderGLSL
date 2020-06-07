#version 450 core

layout (location = 0) in vec4 vertexLoc;

out vec4 vertLoc;


void main()
{
    gl_Position = vertexLoc;
    vertLoc = vertexLoc;
}

