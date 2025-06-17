#version 300 es

precision mediump int;
precision mediump float;
precision mediump usampler2D;
out uvec4 fragColor;

uniform vec2 size;
uniform float frame;
uniform float seed;
uniform usampler2D tex;

#define BLACK uvec3(0)
#define WHITE uvec3(0xFFFFFFFFu)

const ivec2[8] offsets = ivec2[8](ivec2(-1, -1), ivec2(0, -1), ivec2(1, -1),  // top row
ivec2(-1, 0), ivec2(1, 0),  // middle row (skip center)
ivec2(-1, 1), ivec2(0, 1), ivec2(1, 1)   // bottom row
);

uint Pcg(uint v) {
    uint state = v * 747796405u + 2891336453u;
    uint word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

uint at(ivec2 coord) {
    return texelFetch(tex, coord % ivec2(size), 0).r > 0u ? 1u : 0u;
}

void main() {
    ivec2 pos = ivec2(gl_FragCoord.xy);

    if(int(frame) < 4) {
        uvec3 col = (Pcg(uint(pos.x * 2000 + int(seed)) ^ uint(pos.y * 1000 + int(seed))) & 1u) != 0u ? WHITE : BLACK;
        fragColor = uvec4(col, 0xFFFFFFFFu);
        return;
    }

    uint self = at(pos);
    uint liveNeighbors = 0u;

    for(int i = 0; i < 8; i++) {
        uint neighbor = at(pos + offsets[i]);
        liveNeighbors += neighbor;
    }

    if(self != 0u) {
        if(liveNeighbors == 2u || liveNeighbors == 3u)
            self = 1u;
        else
            self = 0u;
    } else {
        if(liveNeighbors == 3u)
            self = 1u;
    }

    fragColor = uvec4(self != 0u ? WHITE : BLACK, 0xFFFFFFFFu);
}