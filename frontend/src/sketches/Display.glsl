#version 300 es
precision highp usampler2D;
precision highp float;
precision highp int;

out vec4 fragColor;

uniform vec2 size;
uniform usampler2D tex;

void main() {
    fragColor = vec4(texelFetch(tex, ivec2(gl_FragCoord.xy), 0)) / vec4(0xFFFFFFFFu);
}