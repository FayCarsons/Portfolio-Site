import {
  add_scroll_callback,
  add_touch_callbacks,
  event_xy,
  map2,
} from "../../lib/utilities.ts";
import { worley_frag_glsl } from "./shaders.ts";

import {
  Shader,
  create_shader,
  run_shader,
  reset_uniforms,
} from "../../lib/shader.ts";
import {
  maximize_canvas,
  canvas_resolution,
  create_context,
} from "../../lib/context.ts";

const SCROLL_FACTOR: number = 0.005;
const TIME_FACTOR: number = 0.001;

let SCROLL_START: [number, number] = [0, 0];
let SCROLL: [number, number] = [0, 0];

export function init_shaders() {
  const canvas: HTMLCanvasElement = document.getElementById(
    "canvas"
  ) as HTMLCanvasElement;

  add_touch_callbacks(canvas, touchstart_callback, touchmove_callback);
  add_scroll_callback(scroll_callback);

  const gl = create_context(canvas);
  maximize_canvas(gl);

  let size = canvas_resolution(gl);

  const uniforms = {
    scroll: [0, 0],
    time: 0,
    resolution: size,
  };

  let worley = create_shader(gl, {
    sources: {
      frag: { glsl: worley_frag_glsl, label: "worley noise frag" },
    },
    uniforms,
  });

  window.addEventListener("resize", function (_: UIEvent) {
    const resolution: [number, number] = [
      window.innerWidth,
      window.innerHeight,
    ];
    worley = reset_uniforms(worley, { ...worley.uniforms, resolution });
    maximize_canvas(gl);
  });

  render(gl, worley);
}

// Render function
function render(gl: WebGL2RenderingContext, worley: Shader) {
  maximize_canvas(gl);

  const uniforms = {
    scroll: SCROLL.map((n) => n * SCROLL_FACTOR),
    resolution: canvas_resolution(gl),
    time: (worley.uniforms.time as number) + TIME_FACTOR,
  };

  worley = reset_uniforms(worley, uniforms);
  run_shader(gl, worley);

  // Request the next frame
  requestAnimationFrame(() => {
    render(gl, worley);
  });
}

const touchstart_callback = (event: TouchEvent) => {
  SCROLL_START = event_xy(event);
};

const touchmove_callback = (event: TouchEvent) => {
  const move = map2(
    event_xy(event),
    SCROLL_START,
    (a: number, b: number) => a - b
  );
  SCROLL = map2(SCROLL, move, (a, b) => a + b) as [number, number];
};

const scroll_callback = (event: WheelEvent) => {
  const delta_mouse = [event.deltaX, event.deltaY];
  SCROLL = map2(SCROLL, delta_mouse, (a: number, b: number) => a + b) as [
    number,
    number,
  ];
};
