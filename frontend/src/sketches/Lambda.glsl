#version 300 es
precision highp float;
out vec4 fragColor;

uniform vec2 size;
uniform float frame;

vec3 palette(float t) {
    return vec3(0.5) + vec3(0.5)*cos(6.283185*(vec3(1.0)*t + vec3(0.0, 0.1, 0.2)));
}

float Box(vec2 pos, vec2 a, vec2 b, float thickness) {
    float l = length(b-a);
    vec2 d = (b-a)/l;
    vec2  q = (pos-(a+b)*0.5);
          q = mat2(d.x,-d.y,d.y,d.x)*q;
          q = abs(q)-vec2(l,thickness)*0.5;
    return length(max(q,0.0)) + min(max(q.x,q.y),0.0); 
}

float map(vec2 uv) {
    uv *= 1.5;
    float longBox = Box(uv, vec2(0.20, -0.3), vec2(-0.125, 0.4), 0.1);
    float shortBox = Box(uv, vec2(-0.2, -0.3), vec2(0.01, 0.1), 0.1);
    float dist = min(longBox, shortBox);
    return dist;
}

void main() {
    vec2 pixelSize = 1.0 / size;
    
    // 4 sample points within the pixel
    vec2 offsets[4] = vec2[](
        vec2(-pixelSize.x, -pixelSize.y),
        vec2(pixelSize.x, -pixelSize.y),
        vec2(-pixelSize.x,  pixelSize.y),
        vec2(pixelSize)
    );

    float total = 0.;
    for (int i = 0; i < 4; i++) {
        vec2 uv = (gl_FragCoord.xy + offsets[i] - 0.5 * size) / size.y;
        total += map(uv);
    }

    total /= 4.;

    vec2 uv = (gl_FragCoord.xy - 0.5 * size.xy) / size.y;
    
    fragColor = total > 0. ? vec4(0) : vec4(palette(length(uv) + sin(frame * 0.005)), 1);
}