// https://learnopengl.com/Advanced-OpenGL/Geometry-Shader

#define M_PI 3.1415926535897932384626433832795

#ifdef VertexShader

layout (location = 0) in float aPos;
layout (location = 1) in float aSpeed;

uniform float numCells;

out VS_OUT {
  vec3 color;
} vs_out;

void main()
{
    float angle = 2.0 * M_PI * aPos ;
    gl_Position = vec4(0.8 * cos(angle), 0.8 * sin(angle), 0.0, 1.0);
//    gl_Position = vec4(2 * aPos - 1 + 1/numCells, 0.0, 0.0, 1.0);
    float base = 0.4;
    float maxSpeed = 5.0;
    float scale = (1 - base) / maxSpeed;
    vs_out.color = vec3(base, base, base) + scale * vec3(aSpeed, 0.0, aSpeed / 2.0);
}

#endif

#ifdef GeometryShader

uniform float numCells;

in VS_OUT {
  vec3 color;
} gs_in[];
out vec3 fColor;

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

void main() {
  float size = 1 / numCells;
  fColor = gs_in[0].color;
  gl_Position = gl_in[0].gl_Position + vec4(-size, -size, 0.0, 0.0);
  EmitVertex();
  gl_Position = gl_in[0].gl_Position + vec4(size, -size, 0.0, 0.0);
  EmitVertex();
  gl_Position = gl_in[0].gl_Position + vec4(-size, size, 0.0, 0.0);
  EmitVertex();
  gl_Position = gl_in[0].gl_Position + vec4(size, size, 0.0, 0.0);
  EmitVertex();
  EndPrimitive();
}


#endif

#ifdef FragmentShader

in vec3 fColor;
out vec4 FragColor;

void main()
{
    FragColor = vec4(fColor, 1.0);
}

#endif
