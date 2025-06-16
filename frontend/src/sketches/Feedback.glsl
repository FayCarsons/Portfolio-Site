#version 300 es
#define UINT_MAX 0xFFFFFFFFu
precision highp float;
precision highp usampler2D;

out uvec4 fragColor;

uniform usampler2D tex;
uniform vec2 size;
uniform float frame;
uniform float seed;

float Circle(vec2 pos, float radius) {
    return length(pos) - radius;
}

float Rect(vec2 pos, vec2 size) {
    vec2 d = abs(pos) - size;
    return length(max(d, 0.0f)) + min(max(d.x, d.y), 0.0f);
}

uint Pcg(uint v) {
    uint state = v * 747796405u + 2891336453u;
    uint word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

// Simple hash for randomness
float hash(float n) {
    return float(Pcg(uint(n + seed))) / float(UINT_MAX);
}

vec2 Hash2(vec2 pos) {
    uvec2 q = uvec2(ivec2(pos + seed));
    uint h1 = Pcg(q.x + q.y * 374761393u);
    uint h2 = Pcg(q.y + q.x * 668265263u);
    return vec2(h1, h2) / float(UINT_MAX);
}

vec3 GetTex(vec2 offset) {
    vec2 sampleCoord = mod(gl_FragCoord.xy + offset, size);
    return vec3(texelFetch(tex, ivec2(sampleCoord), 0).rgb) / float(UINT_MAX);
}

void main() {
    vec2 pos = (gl_FragCoord.xy - 0.5f * size) / size.y;

    vec3 col = vec3(1);

    // Use previous frame with feedback
    vec2 offset = vec2(0);
    int twoSecond = int(frame / 120.f);
    uint second = uint(frame / 60.f);
    uint idx = Pcg(second) % 6u;

    switch(idx) {
        case 0u: {
            offset.y -= (hash(pos.x + frame / 60.f));
            col = GetTex(offset);
            break;
        }
        case 1u: {
            offset.x -= (hash(pos.y + frame / 60.f)) * 4.f;
            offset.y += gl_FragCoord.y > size.y * 0.5f ? hash(pos.x * 10.f + frame / 30.f) * 4.f : 1.f;
            col = GetTex(offset);
            break;
        }
        case 2u: {
            offset = Hash2(1.f + hash(float(second)) * 2.f + pos);
            col = GetTex(offset);
            break;
        }
        // Jerky
        case 3u: {
            offset.y += step(sin(frame / 4.f), 0.f) * 2.f - 1.f + hash(pos.y * 8.f + frame);
            col = GetTex(offset);
            break;
        }
        case 4u: {
            vec2 rectPos = pos - (Hash2(vec2(second, -float(second))) - 0.5f);
            vec2 rectSize = vec2(hash(float(second)) * 0.05f);
            float rect = Rect(rectPos, rectSize);

            vec3 rectColor = Pcg(second) % 1000u < 500u ? vec3(1, 0.01f, 0.01f) : vec3(1, 1, 0.01f);
            col = rect < 0.f ? rectColor : GetTex(offset);  // Changed > to 
            break;
        }
        case 5u: {
            // NEW: Radial displacement from center
            offset.y -= hash(pos.x * (20.f + float(Pcg(second)) / float(UINT_MAX)));

            col = GetTex(offset);
            break;
        }
        default: {
            col = GetTex(offset);
            break;
        }
    }

    if(frame < 4.f || (uint(frame) % 240u == 0u) || hash(frame) > 0.99f) {
        // Use pixel coordinates instead of normalized coordinates for precise control
        // Bottom 2 pixels of the canvas
        col = gl_FragCoord.y < 2.f ? vec3(second % 2u == 0u && second > 5u ? 1.f : 0.f) : col;
    }

    fragColor = uvec4(col * float(UINT_MAX), UINT_MAX);
}