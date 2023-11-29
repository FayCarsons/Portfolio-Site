import { u8_to_vec3 } from "./utilities";

const SKY_BLUE = u8_to_vec3([135, 206, 235]);
const RECT_FACTOR = 0.2;

export const worley_frag_glsl = `#version 300 es
precision highp float;
precision highp int;
precision highp usampler2D;
precision highp sampler2D;

uniform vec2 resolution;
uniform float time;
uniform vec2 scroll;

out vec4 fragColor;

#define inc(x) (x + 1.)
#define dec(x) (x - 1.)

vec4 permute(vec4 x)
{
  return mod((x * (1. + (34. * x))), 289.);
}
uint pcg(uint x)
{
  uint state = ((x * 747796405u) + 2891336453u);
  uint word = (((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u);
  return ((word >> 22u) ^ word);
}
uvec3 pcg(uvec3 x)
{
  x = ((x * 1664525u) + 1013904223u);
  x.x += (x.y * x.z);
  x.y += (x.z * x.x);
  x.z += (x.x * x.y);
  x ^= (x >> 16u);
  x.x += (x.y * x.z);
  x.y += (x.z * x.x);
  x.z += (x.x * x.y);
  return x;
}
float rand_pcg(float p)
{
  return (float(pcg(floatBitsToUint(p))) / float(0xffffffffu));
}
float rand_pcg(vec2 p)
{
  return (float(pcg((pcg(floatBitsToUint(p.x)) + floatBitsToUint(p.y)))) / float(0xffffffffu));
}
vec3 rand_pcg(vec3 p)
{
  return (vec3(pcg(uvec3(floatBitsToUint(p.x), floatBitsToUint(p.y), floatBitsToUint(p.z)))) / float(0xffffffffu));
}
vec4 taylorInvSqrt(vec4 r)
{
  return (1.79284291400159001562 - (r * 0.85373472095313995833));
}
float snoise3D(vec3 v)
{
  vec2 C = vec2((1. / 6.), (1. / 3.));
  vec4 D = vec4(0., 0.5, 1., 2.);
  vec3 i = floor((v + dot(v, C.yyy)));
  vec3 x0 = ((v - i) + dot(i, C.xxx));
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = (1. - g);
  vec3 c1 = min(g.xyz, l.zxy);
  vec3 c2 = max(g.xyz, l.zxy);
  vec3 x1 = ((x0 - c1) + C.xxx);
  vec3 x2 = ((x0 - c2) + (2. * C.xxx));
  vec3 x3 = ((x0 - 1.) + (3. * C.xxx));
  i = mod(i, 289.);
  vec4 p = permute((permute((permute((i.z + vec4(0., c1.z, c2.z, 1.))) + i.y + vec4(0., c1.y, c2.y, 1.))) + i.x + vec4(0., c1.x, c2.x, 1.)));
  vec3 ns = ((D.wyz * (1. / 7.)) - D.xzx);
  vec4 j = (p - (49. * floor((p * ns.z * ns.z))));
  vec4 x_ = floor((j * ns.z));
  vec4 y_ = floor((j - (7. * x_)));
  vec4 x = (ns.yyyy + (ns.x * x_));
  vec4 y = (ns.yyyy + (ns.x * y_));
  vec4 h = (1. - (abs(x) + abs(y)));
  vec4 b0 = vec4(x.xy, y.xy);
  vec4 b1 = vec4(x.zw, y.zw);
  vec4 s0 = (1. + (2. * floor(b0)));
  vec4 s1 = (1. + (2. * floor(b1)));
  vec4 sh = (0. - step(h, vec4(0.)));
  vec4 a0 = (b0.xzyw + (s0.xzyw * sh.xxyy));
  vec4 a1 = (b1.xzyw + (s1.xzyw * sh.zzww));
  vec3 p0 = vec3(a0.xy, h.x);
  vec3 p1 = vec3(a0.zw, h.y);
  vec3 p2 = vec3(a1.xy, h.z);
  vec3 p3 = vec3(a1.zw, h.w);
  vec4 norm = taylorInvSqrt(vec4(dot(p0, p0), dot(p1, p1), dot(p2, p2), dot(p3, p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;
  vec4 m = max((0.5999999999999999778 - vec4(dot(x0, x0), dot(x1, x1), dot(x2, x2), dot(x3, x3))), 0.);
  m *= m;
  return (42. * dot((m * m), vec4(dot(p0, x0), dot(p1, x1), dot(p2, x2), dot(p3, x3))));
}
vec2 getPos()
{
  float minDim = min(resolution.x, resolution.y);
  return ((gl_FragCoord.xy - (0.5 * (resolution - minDim))) / minDim);
}
float worley_noise(vec3 pos)
{
  vec3 id = floor(pos);
  vec3 p = fract(pos);
  float min_dist = 10000.;
  vec3 h = vec3(0.);
  vec3 d = vec3(0.);
  for (int x = -1; x < 2; x++) {
    for (int y = -1; y < 2; y++) {
      for (int z = -1; z < 2; z++) {
        vec3 offset = vec3(x, y, z);
        vec3 h = rand_pcg((id + offset));
        h += offset;
        vec3 d = (p - h);
        min_dist = min(min_dist, dot(d, d));
      }
    }
  }
  return min_dist;
}
float fbm_worley_noise(vec3 x, int octaves, float hurstExponent)
{
  float g = exp2((0. - hurstExponent));
  float f = 1.;
  float a = 1.;
  float t = 0.;
  for (int i = 0; (i < octaves); i++) {
    t += (a * worley_noise((f * x)));
    f *= 2.;
    a *= g;
  }
  return t;
}
void main()
{
  vec2 pos = (gl_FragCoord.xy / resolution);
  pos += (vec2((time * 0.333), (time * 0.0333)) + vec2(scroll.x, (1. - scroll.y)));
  float worley = fbm_worley_noise(vec3(((pos * 1.5) + (0.01 * snoise3D(vec3((pos * 20.), time)))), time), 5, 0.75);
  worley = max(((1. - worley) - ${RECT_FACTOR}) / ${1 - RECT_FACTOR}, 0.);
  vec3 color = mix(${SKY_BLUE}, vec3(1.), worley);
  fragColor = vec4(color, 1.);
}`;

export const render_frag_glsl = `
#version 300 es
precision highp float;
precision highp usampler2D;

uniform vec2 resolution;
uniform sampler2D tex;

out vec4 fragColor;

void main() {
    vec3 color = texelFetch(tex, ivec2(gl_FragCoord.xy), 0).rgb;
    fragColor = vec4(color, 1.);
}
`;
