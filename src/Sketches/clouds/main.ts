import {
  addScrollCallback,
  addTouchCallbacks,
  event_xy,
  map2,
  removeScrollCallback,
  removeTouchCallbacks,
} from '../../lib/utilities.js';
import { worley_frag_glsl } from './shaders.js';

import {
  Shader,
  createShader,
  runShader,
  resetUniforms,
} from '../../lib/shader.js';

import {
  maximizeCanvas,
  canvasResolution,
  createContext,
} from '../../lib/context.js';
import { VertexArrayInfo } from 'twgl.js';

const SCROLL_FACTOR: number = 0.002;
const TIME_FACTOR: number = 0.0015;

let SCROLL_START: [number, number] = [0, 0];
let SCROLL: [number, number] = [0, 0];

let worley: Shader;
let gl: WebGL2RenderingContext;
let frameID: number;

export function initShaders() {
  const canvas: HTMLCanvasElement = document.getElementById(
    'canvas',
  ) as HTMLCanvasElement;

  addTouchCallbacks(document.body, touchStartCallback, touchmoveCallback);
  addScrollCallback(scrollCallback);

  gl = createContext(canvas);
  maximizeCanvas(gl);

  let size = canvasResolution(gl);

  const uniforms = {
    scroll: [0, 0],
    time: 0,
    resolution: size,
  };

  worley = createShader(gl, {
    sources: {
      frag: { glsl: worley_frag_glsl, label: 'worley noise frag' },
    },
    uniforms,
  });

  window.addEventListener('resize', resizeHandler);

  render(gl, worley);
}

// Render function
function render(gl: WebGL2RenderingContext, worley: Shader) {
  const uniforms = {
    scroll: SCROLL.map((n) => n * SCROLL_FACTOR),
    resolution: canvasResolution(gl),
    time: (worley.uniforms.time as number) + TIME_FACTOR,
  };

  worley = resetUniforms(worley, uniforms);
  runShader(gl, worley);

  // Request the next frame
  frameID = requestAnimationFrame(() => {
    render(gl, worley);
  });
}

export const shaderCleanup = () => {
  window.removeEventListener('resize', resizeHandler);
  removeScrollCallback(scrollCallback);
  removeTouchCallbacks(document.body, touchStartCallback, touchmoveCallback);
  gl.deleteVertexArray(
    (worley.data.geometry_buffer as VertexArrayInfo).vertexArrayObject,
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
};

const touchStartCallback = (event: TouchEvent) => {
  SCROLL_START = event_xy(event);
};

const touchmoveCallback = (event: TouchEvent) => {
  const move = map2(
    event_xy(event),
    SCROLL_START,
    (a: number, b: number) => a - b,
  );
  SCROLL = map2(SCROLL, move, (a, b) => a + b) as [number, number];
};

const scrollCallback = (event: WheelEvent) => {
  const delta_mouse = [event.deltaX, event.deltaY];
  SCROLL = map2(SCROLL, delta_mouse, (a: number, b: number) => a + b) as [
    number,
    number,
  ];
};

const resizeHandler = function(_: UIEvent) {
  const resolution: [number, number] = [window.innerWidth, window.innerHeight];
  worley = resetUniforms(worley, { ...worley.uniforms, resolution });
  maximizeCanvas(gl);
};
