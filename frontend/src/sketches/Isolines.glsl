#version 300 es
#define BLACK vec3(0)
#define WHITE vec3(1)

#define LINE_WIDTH 2.
#define LINE_SPACING 0.25
#define EPSILON 0.001

precision highp float;
out vec4 fragColor;

uniform vec2 size;
uniform float frame;

uint Pcg(uint v) {
    uint state = v * 747796405u + 2891336453u;
    uint word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

// 3D version for PCG
uvec3 Pcg3d(uvec3 v) {
    v = v * 1664525u + 1013904223u;
    v.x += v.y * 1664525u;
    v.y += v.z * 1664525u;
    v.z += v.x * 1664525u;
    v = v ^ (v >> 16u);
    v.x += v.y * 1664525u;
    v.y += v.z * 1664525u;
    v.z += v.x * 1664525u;
    v = v ^ (v >> 16u);
    return v;
}

// 3D hash returning 3D gradient
vec3 Hash3(vec3 p) {
    uvec3 q = uvec3(ivec3(p * 1000.0f));
    uvec3 n = Pcg3d(q);
    return vec3(n) / float(0xffffffffu) * 2.0f - 1.0f;
}

// 3D Simplex Noise
float Noise3D(vec3 p) {
    const float K1 = 1.0f / 6.0f;
    const float K2 = 1.0f / 3.0f;

    vec3 i = floor(p + (p.x + p.y + p.z) * K2);
    vec3 x0 = p - i + (i.x + i.y + i.z) * K1;

    vec3 e = step(vec3(0.0f), x0 - x0.yzx);
    vec3 i1 = e * (1.0f - e.zxy);
    vec3 i2 = 1.0f - e.zxy * (1.0f - e);

    vec3 x1 = x0 - i1 + K1;
    vec3 x2 = x0 - i2 + 2.0f * K1;
    vec3 x3 = x0 - 1.0f + 3.0f * K1;

    vec4 h = max(0.6f - vec4(dot(x0, x0), dot(x1, x1), dot(x2, x2), dot(x3, x3)), 0.0f);
    vec4 n = h * h * h * h * vec4(dot(x0, Hash3(i)), dot(x1, Hash3(i + i1)), dot(x2, Hash3(i + i2)), dot(x3, Hash3(i + 1.0f)));

    return dot(n, vec4(32.0f));
}

float Isolines(float value, float spacing, float width) {
    float scaledValue = value / spacing;
    float distToLine = abs(scaledValue - round(scaledValue)) * spacing;

    float grad = length(vec2(dFdx(value), dFdy(value)));
    grad = max(grad, 0.001f); // Just prevent division by zero issues

    // Use a smaller multiplier
    return 1.0f - smoothstep(0.0f, grad * 0.5f * width, distToLine);
}

void main() {
    vec2 uv = (gl_FragCoord.xy / size) * 2.f - 1.f;

    vec3 p3d = vec3(uv * 3.f, frame * 0.0003f);
    float lines = Isolines(Noise3D(p3d), LINE_SPACING, LINE_WIDTH);

    vec3 col = mix(BLACK, WHITE, 1.f - lines);

    fragColor = vec4(col, 1.0f);
}