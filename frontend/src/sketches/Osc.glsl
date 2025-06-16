#version 300 es
precision highp float;
out vec4 fragColor;

uniform float frame;
uniform vec2 size;

float hash(float x) {
    return fract(sin(x * 127.1f) * 43758.5453f);
}

void main() {
    vec2 uv = gl_FragCoord.xy / size;

    float time = frame * 0.01f;

                // Convert to centered coordinates
    vec2 center = (uv - 0.5f) * 2.0f;
    center.x *= size.x / size.y;

    float color = 1.0f; // Start with white background

                // Grid lines
    vec2 grid = fract(uv * 20.0f);
    float gridLines = step(0.98f, max(grid.x, grid.y));
    color *= 1.0f - gridLines * 0.3f; // Subtle grid

                // Main signal wave (complex synthetic data)
    float x = uv.x;
    float signal = 0.0f;

                // Base carrier wave
    signal += sin(x * 20.0f + time * 3.0f) * 0.3f;

                // Modulated component
    signal += sin(x * 8.0f + time * 1.5f) * sin(x * 45.0f + time * 8.0f) * 0.2f;

                // Random noise bursts
    float noiseBurst = step(0.95f, hash(floor(time * 2.0f + x * 10.0f)));
    signal += (hash(x * 100.0f + time * 20.0f) - 0.5f) * noiseBurst * 0.4f;

                // Occasional spike anomalies
    float spike = step(0.98f, sin(time * 0.7f + x * 3.14159f));
    signal += spike * 1.5f;

                // Map signal to screen space (center vertically)
    float signalY = 0.5f + signal * 0.15f;

                // Draw main signal line
    float lineWidth = 3.0f / size.y;
    float mainSignal = step(abs(uv.y - signalY), lineWidth);
    color *= 1.0f - mainSignal;

                // Secondary reference wave
    float refSignal = 0.5f + sin(x * 15.0f + time * 2.0f) * 0.08f;
    float refLine = step(abs(uv.y - refSignal), lineWidth * 0.5f);
    color *= 1.0f - refLine * 0.7f;

                // HUD Elements

                // Border frame
    vec2 border = step(vec2(0.02f), uv) * step(uv, vec2(0.98f));
    float frame = 1.0f - (border.x * border.y);
    color *= 1.0f - frame;

                // Corner brackets
    vec2 cornerTL = step(uv, vec2(0.15f, 0.85f)) * step(vec2(0.05f, 0.8f), uv);
    vec2 cornerBR = step(vec2(0.85f, 0.15f), uv) * step(uv, vec2(0.95f, 0.2f));
    float corners = max(cornerTL.x * cornerTL.y, cornerBR.x * cornerBR.y);
    color *= 1.0f - corners;

                // Center crosshair
    float crosshairH = step(abs(uv.y - 0.5f), 1.0f / size.y) * step(abs(uv.x - 0.5f), 0.05f);
    float crosshairV = step(abs(uv.x - 0.5f), 1.0f / size.x) * step(abs(uv.y - 0.5f), 0.05f);
    color *= 1.0f - max(crosshairH, crosshairV) * 0.5f;

                // Scale markers along edges
    float scaleX = step(0.98f, fract(uv.x * 10.0f)) * step(uv.y, 0.1f) * step(0.05f, uv.y);
    float scaleY = step(0.98f, fract(uv.y * 8.0f)) * step(uv.x, 0.1f) * step(0.05f, uv.x);
    color *= 1.0f - max(scaleX, scaleY) * 0.6f;

                // Trigger indicator (top right)
    vec2 triggerPos = vec2(0.85f, 0.85f);
    float triggerDist = length(uv - triggerPos);
    float trigger = step(triggerDist, 0.02f) * step(0.8f, sin(time * 10.0f));
    color *= 1.0f - trigger;

                // Status dots (top left)
    for(int i = 0; i < 3; i++) {
        vec2 dotPos = vec2(0.1f + float(i) * 0.03f, 0.9f);
        float dotDist = length(uv - dotPos);
        float dotActive = step(0.8f, sin(time * (2.0f + float(i)) + float(i)));
        float dot = step(dotDist, 0.008f) * dotActive;
        color *= 1.0f - dot;
    }

    fragColor = vec4(vec3(color), 1.0f);
}