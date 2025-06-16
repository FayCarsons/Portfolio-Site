#version 300 es
precision highp float;
out vec4 fragColor;

uniform float frame;
uniform vec2 size;

float hexDist(vec2 p) {
    p = abs(p);
    float c = dot(p, normalize(vec2(1.0f, 1.73f)));
    c = max(c, p.x);
    return c;
}

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(127.1f, 311.7f))) * 43758.5453f);
}

void main() {
    vec2 uv = (gl_FragCoord.xy - 0.5f * size) / size.y;

    uv *= 16.0f;

    // Hexagonal grid coordinates
    vec2 gv = uv;

    // Offset every other row
    gv.x += 0.5f * floor(gv.y);

    // Get grid cell ID
    vec2 id = floor(gv);
    gv = fract(gv) - 0.5f;

    // Calculate hexagon distance
    float d = hexDist(gv);

    // Create hexagon cells
    float hexagon = smoothstep(0.f, 0.38f, d);

    // Generate pseudo-random activation based on cell ID and time
    float cellHash = hash(id);
    float timeOffset = cellHash * 6.28f; // Different start times
    float activationTime = sin(frame * 0.008f + timeOffset) * 0.5f + 0.5f;

    // Create wave pattern for sequential activation
    float wave = sin(frame * 0.02f + length(id) * 0.5f) * 0.5f + 0.5f;

    // Combine random and wave activation
    float activation = mix(activationTime, wave, 0.6f);
    activation = pow(activation, 3.0f); // Sharp falloff

    // Pure black and white - no grays, no gradients
    float act = mix(activationTime, wave, 0.6f);

    // Create outer hexagon shape
    float hex = smoothstep(0.41f, 0.39f, d);

    // Create smaller inner hexagon (about half the size)
    float innerHex = smoothstep(0.3f, 0.2f, d);

    // Sharp threshold for pure black/white
    float isActive = step(0.5f, act);

    float outerRing = hex - innerHex; // This gives you just the ring
    float cellColor = outerRing * isActive - innerHex * isActive;

    vec3 finalColor = vec3(cellColor);

    fragColor = vec4(finalColor, 1.0f);
}