#version 300 es

#define RED vec3(1, 0, 0)
#define YELLOW vec3(1, 1, 0)
#define BLUE vec3(0, 0, 1)
#define BLACK vec3(0)
#define WHITE vec3(1)

precision highp float;
precision highp int;

out vec4 fragColor;

uniform vec2 size;
uniform float frame;
uniform float seed;

uint Pcg(uint v) {
    uint state = v * 747796405u + 2891336453u;
    uint word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

uvec2 Pcg(uvec2 v) {
    v = v * 1664525u + 1013904223u;
    v.x += v.y * 1664525u;
    v = v ^ (v >> 16u);
    v.x += v.y * 1664525u;
    v = v ^ (v >> 16u);
    return v;
}

uvec3 Pcg(uvec3 v) {
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

vec2 Hash2(vec2 pos) {
    uvec2 q = uvec2(ivec2(pos + seed));
    uint h1 = Pcg(q.x + q.y * 374761393u);
    uint h2 = Pcg(q.y + q.x * 668265263u);
    return vec2(h1, h2) / float(0xFFFFFFFFu);
}

// 3D hash returning 3D gradient
vec3 Hash3(vec3 p) {
    uvec3 q = uvec3(ivec3(p * 1000.0f));
    uvec3 n = Pcg(q);
    return vec3(n) / float(0xffffffffu) * 2.0f - 1.0f;
}

float Hollow(float sdf, float thickness) {
    return abs(sdf) - thickness;
}

float Smooth(float sdf, float edge) {
    return smoothstep(-edge, edge, sdf);
}

float Scanner(vec2 pos, float y, float thickness) {
    float dist = abs(pos.y - y);
    return (1.0f - smoothstep(0.f, thickness * 0.5f, dist)) * exp(-abs(pos.y) * 2.f);
}

float EaseInOut(float t) {
    return t * t * (3.f - 2.f * t);
}

float EaseOutQuad(float t) {
    return 1.f - pow(1.f - t, 2.f);
}

float Line(vec2 p, vec2 a, vec2 b) {
    vec2 pa = p - a, ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0f, 1.0f);
    return length(pa - ba * h);
}

float aastep(float threshold, float value) {
    float afwidth = 10.0f / size.y; // pixel width for smoothstep edge 
    return smoothstep(threshold - afwidth, threshold + afwidth, value);
}

float fill(float f, float size) {
    return 1.f - aastep(size, f);
}

// Thank u RiceFields https://www.shadertoy.com/view/sdSXRK
vec3 Voronoi(vec2 uv) {
    vec2 iuv = floor(uv);
    vec2 fuv = fract(uv);

    // miniunm distance feature point
    vec2 mNeighbour; // feature point cell
    vec2 mDiff; // vector from current point to feature point
    float mDist = 2.0f;

    // first find the least distant feature point
    for(int j = -1; j <= 1; ++j) {
        for(int i = -1; i <= 1; ++i) {
            vec2 neighbour = vec2(float(i), float(j));
            vec2 point = Hash2(iuv + neighbour);

            vec2 diff = neighbour + point - fuv;

            float dist = dot(diff, diff); // lenght^2

            if(dist < mDist) {
                mDiff = diff;
                mNeighbour = neighbour;
                mDist = dist;
            }
        }
    }

    mDist = 2.0f;
    // second, neighbour search centered at cell contaiting least distant feature point
    for(int j = -2; j <= 2; ++j) {
        for(int i = -2; i <= 2; ++i) {
            // 5x5

            vec2 neighbour = mNeighbour + vec2(float(i), float(j));
            vec2 point = Hash2(iuv + neighbour);

            vec2 diff = neighbour + point - fuv;

            float dist = dot(0.5f * (mDiff + diff), normalize(diff - mDiff)); // distance with projection

            mDist = min(mDist, dist);
        }
    }

    return vec3(fill(mDist, 0.02f));
}

void main() {
    vec2 uv = (gl_FragCoord.xy - 0.5f * size) / size.y;

    vec3 cells = Voronoi(uv * 15.f);

    float t = mod(frame * 0.005f, 1.5f);
    float line = t <= 1.0f ? Scanner(uv, t - 0.5f, 0.15f) : 0.0f;

    vec3 col = WHITE;
    col = mix(col, BLACK, line * cells);

    fragColor = vec4(col, 1.0f);
}