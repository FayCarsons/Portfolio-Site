#version 300 es
precision highp float;

out vec4 fragColor;

uniform vec2 size;
uniform float frame;
uniform float seed;

#define BLACK vec3(0)
#define WHITE vec3(1)

const mat2 m = mat2(1.6f, 1.2f, -1.2f, 1.6f);

uint Pcg(uint v) {
    uint state = v * 747796405u + 2891336453u;
    uint word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

// Simple hash for randomness
float hash(float n) {
    return float(Pcg(uint(n + seed))) / float(0xFFFFFFFFu);
}

float Hash2(vec2 pos) {
    uvec2 q = uvec2(ivec2(pos + seed));
    return float(Pcg(q.x + q.y * 374761393u)) / float(0xFFFFFFFFu);
}

float Noise(vec2 st) {
    vec2 i = floor(st);
    vec2 f = fract(st);

    float a = Hash2(i);
    float b = Hash2(i + vec2(1.0f, 0.0f));
    float c = Hash2(i + vec2(0.0f, 1.0f));
    float d = Hash2(i + vec2(1.0f, 1.0f));

    vec2 u = f * f * (3.0f - 2.0f * f);

    return mix(a, b, u.x) +
        (c - a) * u.y * (1.0f - u.x) +
        (d - b) * u.x * u.y;
}

// Fractional Brownian Motion for clouds
float Fbm(vec2 st, int octaves) {
    float value = 0.0f;
    float amplitude = 0.5f;
    float frequency = 1.0f;

    for(int i = 0; i < 8; i++) {
        if(i >= octaves)
            break;
        value += amplitude * Noise(st * frequency);
        frequency *= 2.0f;
        amplitude *= 0.5f;
    }
    return value;
}

            // Dithering pattern
float Dither(vec2 pos) {
    vec2 grid = floor(pos * 64.0f);
    return fract(sin(dot(grid, vec2(12.9898f, 78.233f))) * 43758.5f);
}

 // SDF functions
float Box(vec2 p, vec2 b) {
    vec2 d = abs(p) - b;
    return length(max(d, 0.0f)) + min(max(d.x, d.y), 0.0f);
}

float Circle(vec2 p, float r) {
    return length(p) - r;
}

float HUD(vec2 pos) {
    float hud = 0.f;

   // Center dot
    hud = max(hud, 1.f - smoothstep(0.006f, 0.010f, length(pos)));

   // Extensions
    float extH = smoothstep(0.002f, 0.0f, abs(pos.y)) *
        smoothstep(0.06f, 0.08f, abs(pos.x)) *
        (1.f - smoothstep(0.15f, 0.17f, abs(pos.x)));
    float extV = smoothstep(0.002f, 0.0f, abs(pos.x)) *
        smoothstep(0.06f, 0.08f, abs(pos.y)) *
        (1.f - smoothstep(0.15f, 0.17f, abs(pos.y)));
    hud = max(hud, extH + extV);

   // Viewfinder frame
    float frameOuter = 1.0f - smoothstep(0.002f, 0.006f, abs(Box(pos, vec2(0.4f, 0.3f))));
    float frameInner = 1.f - abs(Box(pos, vec2(0.4f, 0.3f)));
    hud = max(hud, frameOuter * frameInner);

    vec2 cornerUR = abs(pos - vec2(0.35f, 0.25f));
    vec2 cornerUL = abs(pos - vec2(-0.35f, 0.25f));
    vec2 cornerLR = abs(pos - vec2(0.35f, -0.25f));
    vec2 cornerLL = abs(pos - vec2(-0.35f, -0.25f));

    float brackets = 0.0f;
   // Upper right
    brackets = max(brackets, smoothstep(0.032f, 0.028f, cornerUR.x) * smoothstep(0.004f, 0.0f, cornerUR.y));
    brackets = max(brackets, smoothstep(0.004f, 0.0f, cornerUR.x) * smoothstep(0.032f, 0.028f, cornerUR.y));
   // Upper left  
    brackets = max(brackets, smoothstep(0.032f, 0.028f, cornerUL.x) * smoothstep(0.004f, 0.0f, cornerUL.y));
    brackets = max(brackets, smoothstep(0.004f, 0.0f, cornerUL.x) * smoothstep(0.032f, 0.028f, cornerUL.y));
   // Lower right
    brackets = max(brackets, smoothstep(0.032f, 0.028f, cornerLR.x) * smoothstep(0.004f, 0.0f, cornerLR.y));
    brackets = max(brackets, smoothstep(0.004f, 0.0f, cornerLR.x) * smoothstep(0.032f, 0.028f, cornerLR.y));
   // Lower left
    brackets = max(brackets, smoothstep(0.032f, 0.028f, cornerLL.x) * smoothstep(0.004f, 0.0f, cornerLL.y));
    brackets = max(brackets, smoothstep(0.004f, 0.0f, cornerLL.x) * smoothstep(0.032f, 0.028f, cornerLL.y));

    hud = max(hud, brackets);

   // Range rings
    float range1 = abs(length(pos) - 0.2f);
    float range2 = abs(length(pos) - 0.35f);
    hud = max(hud, 1.0f - smoothstep(0.0f, 0.005f, range1));
    hud = max(hud, 1.0f - smoothstep(0.0f, 0.005f, range2));

   // Tick marks on frame
    float angle = atan(pos.y, pos.x);
    float tickPattern = smoothstep(0.92f, 0.98f, cos(angle * 6.0f));
    float atFrame = 1.0f - smoothstep(0.015f, 0.025f, abs(length(pos) - 0.4f));
    hud = max(hud, tickPattern * atFrame);

    return hud;
}

void main() {
    vec2 pos = (gl_FragCoord.xy - 0.5f * size) / size.y;

    vec3 col = vec3(1);

    vec2 cloudPos = pos * 5.f + vec2(frame * 0.005f, 0);
    vec2 warp = vec2(Noise(cloudPos * 0.5f) * 0.3f, Noise(cloudPos * 0.5f + 100.f) * 0.2f);

    float clouds = max(Fbm(cloudPos + warp, 4) - 0.1f, 0.f);

    float ditherPattern = Dither(gl_FragCoord.xy);
    float thresh = 0.55f + (ditherPattern - 0.5f) * 0.2f;
    clouds = step(thresh, clouds);

    col = mix(WHITE, BLACK, clouds * 0.8f);

    float hud = HUD(pos);

    col = mix(col, BLACK, hud);

    fragColor = vec4(col, 1);
}