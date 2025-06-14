#version 300 es

#define RED vec3(1, 0, 0)
#define YELLOW vec3(1, 1, 0)
#define BLUE vec3(0, 0, 1)
#define BLACK vec3(0)
#define WHITE vec3(1)

precision highp float;
out vec4 fragColor;

uniform vec2 size;
uniform float frame;

float Triangle(vec2 pos, float size) {
    const float k = sqrt(3.0f);
    pos.x = abs(pos.x) - size;
    pos.y = pos.y + size / k;
    if(pos.x + k * pos.y > 0.0f)
        pos = vec2(pos.x - k * pos.y, -k * pos.x - pos.y) / 2.0f;
    pos.x -= clamp(pos.x, -2.0f * size, 0.0f);
    return -length(pos) * sign(pos.y);
}

float Circle(vec2 pos, float radius) {
    return length(pos) - radius;
}

vec2 Repetition(in vec2 p, in vec2 s) {
    vec2 q = p - s * round(p / s);
    return q;
}

float Hollow(float sdf, float thickness) {
    return abs(sdf) - thickness;
}

float Smooth(float sdf, float edge) {
    return smoothstep(-edge, edge, sdf);
}

vec3 Circles(vec2 uv) {
    #define SMOOTH 0.3
    #define SIZE_A 0.0075
    #define SIZE_B 0.02
    #define SIZE_C 0.01

    float t = frame * 0.0065f;

    vec2 posA = vec2(cos(t) * 0.4f + cos(t * 3.0f) * 0.15f, sin(t) * 0.4f + sin(t * 3.0f) * 0.15f);

    vec2 posB = vec2(cos(t * 1.3f) * 0.5f + cos(t * 4.0f) * 0.1f, sin(t * 0.7f) * 0.3f + sin(t * 2.5f) * 0.2f);

    vec2 posC = vec2(cos(t * 0.8f) * 0.3f + cos(t * 5.0f) * 0.12f, sin(t * 1.1f) * 0.45f + sin(t * 1.8f) * 0.18f);

    // Calculate distances and invert the smooth function
    float circleA = 1.0f - Smooth(Circle(uv - posA, SIZE_A), SMOOTH);
    float circleB = 1.0f - Smooth(Circle(uv - posB, SIZE_B), SMOOTH);
    float circleC = 1.0f - Smooth(Circle(uv - posC, SIZE_C), SMOOTH);

    return vec3(circleA, circleB, circleC);
}

void main() {
    vec2 uv = (gl_FragCoord.xy - 0.5f * size) / size.y;

    vec2 repeatedPos = Repetition(uv, vec2(0.09f, 0.095f));
    float triangle = Triangle(repeatedPos, 0.04f);

    float outline = Hollow(triangle, 0.001f);
    float smoothOutline = Smooth(outline, 0.003f);

    float triangleMask = 1.0f - smoothOutline;

    vec3 col = WHITE;
    vec3 circles = Circles(uv);

    col = mix(col, RED, triangleMask * circles.x);
    col = mix(col, YELLOW, triangleMask * circles.y);
    col = mix(col, BLUE, triangleMask * circles.z);

    fragColor = vec4(col, 1.0f);
}