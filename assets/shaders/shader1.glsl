// https://learnopengl.com/Advanced-OpenGL/Geometry-Shader

#ifdef VertexShader

layout (location = 0) in float aPos;
layout (location = 1) in float aSpeed;

out VS_OUT {
  vec3 color;
} vs_out;

void main()
{
    gl_Position = vec4(2 * aPos - 1, 0.0, 0.0, 1.0);
    float base = 0.4;
    float maxSpeed = 5.0;
    float scale = (1 - base) / maxSpeed;
    vs_out.color = vec3(base, base, base) + scale * vec3(aSpeed, 0.0, aSpeed / 2.0);
}

#endif

#ifdef GeometryShader

in VS_OUT {
  vec3 color;
} gs_in[];
out vec3 fColor;

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

void main() {
  float size = 0.03;
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
