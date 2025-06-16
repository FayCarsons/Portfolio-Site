#version 300 es
precision highp float;

out vec4 fragColor;

#define BLACK vec3(0)
#define WHITE vec3(1)
#define TARGET_RED vec3(0.9, 0.1, 0.1)

uniform vec2 size;
uniform float frame;
uniform float seed;

const float ViewRadius = 0.4f;
const float CycleLength = 390.0f; // Extended to ~9 seconds at 30fps

uint Pcg(uint v) {
    uint state = v * 747796405u + 2891336453u;
    uint word = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;
    return (word >> 22u) ^ word;
}

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

float Line(in vec2 p, in vec2 a, in vec2 b, float th) {
    float l = length(b - a);
    vec2 d = (b - a) / l;
    vec2 q = (p - (a + b) * 0.5f);
    q = mat2(d.x, -d.y, d.y, d.x) * q;
    q = abs(q) - vec2(l, th) * 0.5f;
    return length(max(q, 0.0f)) + min(max(q.x, q.y), 0.0f);
}

// Coastal terrain with water, land masses, and islands
vec3 Land(vec2 pos) {
    // Base water level - most of the view should be water (white)
    float baseWater = Fbm(pos * 2.0f, 3) * 0.6f + 0.2f;

    // Land masses - fewer, more defined chunks
    float landMasses = Fbm(pos * 3.0f + vec2(1.5f, 2.3f), 4) * 0.8f;
    float landDetail = Fbm(pos * 8.0f + vec2(3.1f, 1.7f), 3) * 0.3f;

    // Create distinct land areas above water level
    float land = landMasses + landDetail;
    float isLand = step(0.6f, land);

    // Add some smaller islands and coastal features
    float islands = Fbm(pos * 6.0f + vec2(7.2f, 4.8f), 3) * 0.4f;
    float smallIslands = step(0.75f, islands);

    // Combine land features
    float finalLand = max(isLand, smallIslands);

    // Create coastal indentations and bays
    float coastDetail = Fbm(pos * 12.0f + vec2(5.1f, 8.9f), 2) * 0.2f;
    finalLand = step(0.3f + coastDetail, finalLand);

    return finalLand > 0.5f ? BLACK : WHITE;
}

#define LineLen 0.05
#define LineThick 0.008

float Crosshair(vec2 pos) {
    float cross = min(Line(pos, vec2(0, -LineLen), vec2(0, LineLen), LineThick), Line(pos, vec2(-LineLen, 0), vec2(LineLen, 0), LineThick));
    float centerDot = length(pos) - 0.002f;
    return min(cross, centerDot);
}

float Dot(vec2 pos, float radius) {
    return length(pos) - radius;
}

vec2 GetTargetPos(int targetIndex, float cycleSeed) {
    float angle = hash(cycleSeed + float(targetIndex) * 7.3f) * 6.28318f;
    float radius = (hash(cycleSeed + float(targetIndex) * 13.7f) * 0.4f + 0.25f) * ViewRadius;
    return vec2(cos(angle), sin(angle)) * radius;
}

// Enhanced search patterns with longer duration
vec2 SearchPattern(float searchTime, float cycleSeed) {
    float patternType = hash(cycleSeed * 3.7f);

    if(patternType < 0.33f) {
        // Expanding spiral search
        float spiralSpeed = 0.8f;
        float spiralRadius = 0.05f + 0.2f * (searchTime / 5.0f); // Gradually expand
        float angle = searchTime * spiralSpeed;
        return vec2(cos(angle), sin(angle)) * spiralRadius;

    } else if(patternType < 0.66f) {
        // Methodical scanning pattern
        float scanSpeed = 0.4f;
        float x = mod(searchTime * scanSpeed, 3.0f) - 1.5f;
        float y = floor(searchTime * scanSpeed / 3.0f) * 0.15f - 0.3f;
        y = mod(y + 0.6f, 0.6f) - 0.3f; // Wrap vertically
        x = x * x * sign(x) * 0.15f; // Smooth scanning motion
        return vec2(x, y);

    } else {
        // Perimeter sweep
        float sweepSpeed = 0.6f;
        float angle = searchTime * sweepSpeed;
        float radius = 0.25f + 0.1f * sin(searchTime * 0.5f);
        return vec2(cos(angle), sin(angle)) * radius;
    }
}

void main() {
    vec2 pos = (gl_FragCoord.xy - 0.5f * size) / size.y;
    float view = length(pos) - ViewRadius;
    float edge = smoothstep(-0.1f, 0.11f, view);

    if(edge > 0.5f) {
        fragColor = vec4(WHITE, 1);
        return;
    }

    // Pure monotone terrain
    vec3 col = Land(pos);

    // Cycle management
    float cycleTime = mod(frame, CycleLength);
    float cycleSeed = floor(frame / CycleLength) * 12.3f + seed;

    // Phases
    float searchPhase = 150.0f;  // 5 seconds 
    float targetPhase = 120.0f;  // 4 seconds  
    float bombPhase = 30.0f;     // 1 second

    vec2 crosshairPos = vec2(0);
    float crosshairVisible = 1.0f;

    if(cycleTime < searchPhase) {
        // Phase 1: Extended searching motion
        float searchTime = cycleTime / 30.0f;
        crosshairPos = SearchPattern(searchTime, cycleSeed);

    } else if(cycleTime < searchPhase + targetPhase) {
        // Phase 2: Target acquisition
        float targetTime = cycleTime - searchPhase;
        int numTargets = 4;

        // Show targets with fade in/out instead of size pulsing
        for(int i = 0; i < 6; i++) {
            if(i >= numTargets)
                break;

            float targetAppearTime = float(i) * 15.0f;
            if(targetTime >= targetAppearTime) {
                vec2 targetPos = GetTargetPos(i, cycleSeed);

                // Calculate fade based on time since appearance
                float targetAge = (targetTime - targetAppearTime) / 30.0f;
                float fadePattern = 0.8f + 0.2f * sin(targetAge * 6.0f); // Smooth fade in/out

                // Static target dot
                float targetDot = Dot(pos - targetPos, 0.015f);
                col = mix(col, TARGET_RED, step(targetDot, 0.0f) * fadePattern);
            }
        }

        // Crosshair moves between targets
        float lockTime = targetTime - 30.0f;
        if(lockTime > 0.0f) {
            int currentTarget = int(lockTime / 15.0f);
            currentTarget = min(currentTarget, numTargets - 1);

            float targetProgress = fract(lockTime / 15.0f);

            crosshairPos = GetTargetPos(currentTarget, cycleSeed);
        }

    } else {
        // Phase 3: Bombing sequence
        float bombTime = cycleTime - searchPhase - targetPhase;

        crosshairPos = vec2(0);
        crosshairVisible = step(0.5f, fract(bombTime * 0.1f));

        // Simple expanding blast circle - pure black and white
        float blastStart = 15.0f;
        if(bombTime >= blastStart) {
            float blastTime = bombTime - blastStart;
            float blastRadius = blastTime * 0.025f;
            float blastThickness = 0.008f;

            float blastCircle = abs(length(pos) - blastRadius);
            float blastMask = step(blastCircle, blastThickness);
            col = mix(col, WHITE - col, blastMask); // Invert colors at blast ring
        }
    }

    // Draw crosshair with terrain-based color inversion
    float crosshairDist = Crosshair(pos - crosshairPos);
    if(step(crosshairDist, 0.0f) * crosshairVisible > 0.5f) {
        col = mix(col, 1.f - col, step(crosshairDist, 0.f));
    }

    // Clean border - no gradients
    float border = abs(view);
    col = mix(col, BLACK, step(border, 0.005f));

    fragColor = vec4(col, 1.0f);
}