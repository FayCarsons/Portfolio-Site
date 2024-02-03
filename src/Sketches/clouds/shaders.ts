import { uint_to_vec3 } from "../../lib/utilities";

const SKY_BLUE = uint_to_vec3(135, 206, 235);
const SEED = Math.random() * 0xFFFFFFFF;
const RECT_FACTOR = 0.15;

export const worley_frag_glsl = `#version 300 es
precision highp float;uniform vec2 resolution;uniform float time;uniform vec2 scroll;out vec4 fragColor;uvec3 pcg(uvec3 x){x = ((x * 1664525u) + 1013904223u);x.x += (x.y * x.z);x.y += (x.z * x.x);x.z += (x.x * x.y);x ^= (x >> 16u);x.x += (x.y * x.z);x.y += (x.z * x.x);x.z += (x.x * x.y);return x;}vec3 rand_pcg(vec3 p){return (vec3(pcg(uvec3(floatBitsToUint(p.x), floatBitsToUint(p.y), floatBitsToUint(p.z)))) / float(0xffffffffu));}vec2 getPos(){float minDim = min(resolution.x, resolution.y);return ((gl_FragCoord.xy - (0.5 * (resolution - minDim))) / minDim);}float worley_noise(vec3 pos){vec3 id = floor(pos);vec3 p = fract(pos);float min_dist = 10000.;vec3 h = vec3(0.);vec3 d = vec3(0.);for (int x = -1; x < 2; x++) {for (int y = -1; y < 2; y++) {for (int z = -1; z < 2; z++) {vec3 offset = vec3(x, y, z);vec3 h = rand_pcg((id + offset));h += offset;vec3 d = (p - h);min_dist = min(min_dist, dot(d, d));}}}return min_dist;}float fbm_worley_noise(vec3 x, int octaves, float hurstExponent){float g = exp2((0. - hurstExponent));float f = 1.;float a = 1.;float t = 0.;for (int i = 0; (i < octaves); i++) {t += (a * worley_noise((f * x)));f *= 2.;a *= g;}return t;}
void main()
{
  vec2 pos = getPos();
  pos += vec2(time * 0.333, time * 0.05) + vec2(scroll.x, 1. - scroll.y);
  float worley = fbm_worley_noise(vec3(pos * 1.5 , ${SEED}), 5, 0.75);
  worley = max(((1. - worley) - ${RECT_FACTOR}) / ${1 - RECT_FACTOR}, 0.);
  vec3 color = mix(${SKY_BLUE}, vec3(1.), worley);
  fragColor = vec4(pow(color, vec3(1.8)), 1.);
}`;
