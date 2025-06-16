#version 300 es
precision highp float;

out vec4 fragColor;

uniform vec2 size;
uniform float frame;

float scale = 20.f;

vec3 color(vec2 fragCoord) {
    vec2 pos = (fragCoord - 0.5f * size.xy) / size.y;
    vec2 prime = fract(pos * scale) - 0.5f;
    prime *= 1.f + sin(frame * 0.03f - length(pos) * scale) * 0.5f;

    float a = 1.0f - smoothstep(scale * 0.01f - scale * (1.f / size.y), scale * 0.01f, length(prime));

    return vec3(1.0f) * (1.0f - a);
}

void main() {
    vec3 col = vec3(0);

    for(int x = 0; x < 2; x++) {
        for(int y = 0; y < 2; y++) {
            vec2 offset = vec2(x, y);
            col += color(gl_FragCoord.xy + offset);
        }
    }

    col /= 4.f;
    fragColor = vec4(col, 1.f);
}