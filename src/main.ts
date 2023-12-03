import {
  add_scroll_callback,
  add_touch_callbacks,
  event_xy,
  map,
} from "./utilities.ts";
import { worley_frag_glsl } from "./shaders.ts";

import { Shader, ShaderType } from "./shader.ts";
import { Context } from "./context.ts";

console.log('GRAPHICS AND CODE DESIGNED BY FAY CARSONS 2023 \n HOSTED ON THE RASPBERRY PI IN MY BEDROOM')

interface State {
  context: Context;
  shaders: { [key: string]: Shader };
}

const SCROLL_FACTOR: number = 0.005;
const TIME_FACTOR: number = 0.001;

let SCROLL_START: [number, number] = [0, 0];
let SCROLL: [number, number] = [0, 0];

function init() {
  const canvas: HTMLCanvasElement = document.getElementById(
    "canvas"
  ) as HTMLCanvasElement;

  add_touch_callbacks(canvas, touchstart_callback, touchmove_callback);
  add_scroll_callback(scroll_callback);

  const ctx = new Context(canvas);
  ctx.maximize_canvas();

  const uniforms = {
    scroll: [0, 0],
    time: 0,
    resolution: [0, 0],
  };
  const worley_frag = new Shader(ctx, {
    sources: {
      frag: { glsl: worley_frag_glsl, label: "worley noise fragment shader" },
    },
    type: ShaderType.Purefrag,
    uniforms,
  });
  //worley_frag.add_target(ctx, { format: ctx.gl.RGBA, mag: ctx.gl.LINEAR });


  window.addEventListener("resize", function (this: Window, _: UIEvent) {
    const resolution: [number, number] = [this.innerWidth, this.innerHeight];
    worley_frag.reset_uniforms({ resolution, ...worley_frag.uniforms });
    ctx.maximize_canvas();
  });

  const app_state = {
    context: ctx,
    shaders: { worley: worley_frag },
  };

  render(app_state);
}

// Render function
function render({ context, shaders }: State): void {
  const { worley } = shaders;

  context.maximize_canvas();

  const uniforms = {
    scroll: SCROLL.map((n) => n * SCROLL_FACTOR),
    resolution: context.resolution,
    time: worley.uniforms.time + TIME_FACTOR,
  };
  worley.reset_uniforms(uniforms);

  worley.render(context);

  // Request the next frame
  requestAnimationFrame(() => {
    render({ context, shaders });
  });
}

const touchstart_callback = (event: TouchEvent) => {
  SCROLL_START = event_xy(event);
};

const touchmove_callback = (event: TouchEvent) => {
  const move = map(
    event_xy(event),
    SCROLL_START,
    (a: number, b: number) => a - b
  );
  SCROLL = map(SCROLL, move, (a, b) => a + b) as [number, number];
};

const scroll_callback = (event: WheelEvent) => {
  const delta_mouse = [event.deltaX, event.deltaY];
  SCROLL = map(SCROLL, delta_mouse, (a: number, b: number) => a + b) as [
    number,
    number
  ];
};

init();
