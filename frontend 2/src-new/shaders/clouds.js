import {
  addScrollCallback,
  addTouchCallbacks,
  eventXY,
  map2,
  uintToVec3,
} from "../utilities.js";
import * as twgl from "twgl.js";

const SCROLL_FACTOR = 0.002;
const TIME_FACTOR = 0.0015;
const TEX_SCALE_FACTOR = 0.5;

let SCROLL_START = [0, 0];
let SCROLL = [0, 0];

const SKY_BLUE = uintToVec3(135, 206, 235);
const SEED = Math.random() * 0xffffffff;
// Rectification factor
const RECT_FACTOR = 0.15;

const trivialVert = `#version 300 es
void main() {
    vec2 pos[3] = vec2[3]( 
        vec2(-1, 3),
        vec2(-1),
        vec2(3, -1)
    );

    gl_Position = vec4(pos[gl_VertexID], 0, 1);
}`;

const getPos = `vec2 getPos(){
  float minDim = min(size.x, size.y);
  return ((gl_FragCoord.xy - (0.5 * (size - minDim))) / minDim);
}`;

function worleyFrag(...args) {
  const deps = args.join("\n");
  return `#version 300 es
precision highp float;
uniform vec2 size;
uniform float time;
uniform vec2 scroll;
out vec4 fragColor;

uvec3 pcg(uvec3 x) {
  x = ((x * 1664525u) + 1013904223u);
  x.x += (x.y * x.z);x.y += (x.z * x.x);
  x.z += (x.x * x.y);
  x ^= (x >> 16u);
  x.x += (x.y * x.z);
  x.y += (x.z * x.x);
  x.z += (x.x * x.y);
  return x;
}

${deps}

vec3 rand_pcg(vec3 p) {
  return (vec3(pcg(uvec3(floatBitsToUint(p.x), floatBitsToUint(p.y), floatBitsToUint(p.z)))) / float(0xffffffffu));
}



float worley_noise(vec3 pos){
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

float fbm_worley_noise(vec3 x, int octaves, float hurstExponent){
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
  vec2 pos = getPos();
  pos += vec2(time * 0.333, time * 0.05) + vec2(scroll.x, 1. - scroll.y);
  float worley = fbm_worley_noise(vec3(pos * 1.5 , ${SEED}), 5, 0.75);
  worley = max(((1. - worley) - ${RECT_FACTOR}) / ${1 - RECT_FACTOR}, 0.);
  vec3 color = mix(${SKY_BLUE}, vec3(1.), worley);
  fragColor = vec4(pow(color, vec3(1.8)), 1.);
}`;
}

function upscaleFrag(...args) {
  const deps = args.join("\n");
  return `#version 300 es
  precision highp float;
  uniform vec2 size;
  uniform sampler2D tex;
  out vec4 fragColor;

  ${deps}

  void main() {
    vec2 pos = gl_FragCoord.xy / size;
    fragColor = texture(tex, pos);
  } 
`;
}

export default function init() {
  const canvas = document.getElementById("canvas");

  addTouchCallbacks(document.body, touchStartCallback, touchmoveCallback);
  addScrollCallback(scrollCallback);

  const gl = canvas.getContext("webgl2");
  const width = window.innerWidth;
  const height = window.innerHeight;
  gl.canvas.width = width;
  gl.canvas.height = height;

  const worley = {};

  worley.uniforms = {
    scroll: [0, 0],
    time: 0,
    size: [width * TEX_SCALE_FACTOR, height * TEX_SCALE_FACTOR],
  };

  worley.programs = {
    worley: twgl.createProgramInfo(gl, [trivialVert, worleyFrag(getPos)]),
    render: twgl.createProgramInfo(gl, [trivialVert, upscaleFrag(getPos)]),
  };

  const attachments = [
    {
      format: gl.RGBA,
      type: gl.UNSIGNED_BYTE,
      min: gl.LINEAR, // LINEAR filtering for smooth upscaling
      mag: gl.LINEAR,
      wrap: gl.WRAP,
    },
  ];
  worley.texture = twgl.createFramebufferInfo(
    gl,
    attachments,
    width * TEX_SCALE_FACTOR,
    height * TEX_SCALE_FACTOR,
  );

  window.addEventListener("resize", createResizeHandler(gl, worley));
  render(gl, worley);
}

// Render function
function render(gl, worley) {
  const width = window.innerWidth;
  const height = window.innerHeight;

  worley.uniforms.scroll = SCROLL.map((n) => n * SCROLL_FACTOR);
  worley.uniforms.size = [width * TEX_SCALE_FACTOR, height * TEX_SCALE_FACTOR];
  worley.uniforms.time += TIME_FACTOR;

  // Render worley to texture
  gl.bindFramebuffer(gl.FRAMEBUFFER, worley.texture.framebuffer);
  gl.viewport(0, 0, worley.texture.width, worley.texture.height);
  gl.useProgram(worley.programs.worley.program);
  twgl.setUniforms(worley.programs.worley, worley.uniforms);
  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 3);

  // Render tex to screen
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);
  gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);

  gl.useProgram(worley.programs.render.program);
  twgl.setUniforms(worley.programs.render, {
    tex: worley.texture.attachments[0],
    size: [width, height],
  });
  gl.drawArrays(gl.TRIANGLES, 0, 3);

  // Request the next frame
  requestAnimationFrame(function () {
    render(gl, worley);
  });
}

/*
export const shaderCleanup = () => {
  window.removeEventListener('resize', resizeHandler);
  removeScrollCallback(scrollCallback);
  removeTouchCallbacks(document.body, touchStartCallback, touchmoveCallback);
  gl.deleteVertexArray(
    (worley.data.geometry_buffer).vertexArrayObject,
  );
  worley.target?.framebuffer.attachments.forEach((x) =>
    x instanceof WebGLRenderbuffer
      ? gl.deleteRenderbuffer(x)
      : gl.deleteTexture(x),
  );
  gl.deleteProgram(worley.program.program);
  SCROLL = [0, 0];
  SCROLL_START = [0, 0];
  cancelAnimationFrame(frameID);
  return;
}
*/

function add(a, b) {
  return a + b;
}

function sub(a, b) {
  return a - b;
}

function touchStartCallback(event) {
  SCROLL_START = eventXY(event);
}

function touchmoveCallback(event) {
  const move = map2(eventXY(event), SCROLL_START, sub);

  SCROLL = map2(SCROLL, move, add);
}

function scrollCallback(event) {
  const delta_mouse = [event.deltaX, event.deltaY];
  SCROLL = map2(SCROLL, delta_mouse, add);
}

function createResizeHandler(context, shader) {
  return () => {
    const width = window.innerWidth;
    const height = window.innerHeight;

    const size = [width * TEX_SCALE_FACTOR, height * TEX_SCALE_FACTOR];
    twgl.resizeFramebufferInfo(context, shader.texture);
    shader.uniforms = { ...shader.uniforms, size };
    context.canvas.width = width;
    context.canvas.height = height;
  };
}

document.addEventListener("DOMContentLoaded", () => {
  init();
});
