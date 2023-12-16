type MaybeContext = WebGL2RenderingContext | null;
type TouchCallback = (event: TouchEvent) => void;

type Color = [number, number, number]

const webgl2_supported = (gl: MaybeContext) => {
  if (!gl) {
    console.error(
      "Unable to initialize WebGL. Your browser may not support it."
    );
    throw new Error("WEBGL2 NOT SUPPORTED");
  }
};

const check_context = (gl: MaybeContext) => {
  try {
    webgl2_supported(gl);
    return gl as WebGL2RenderingContext;
  } catch (err) {
    alert(err);
    return null;
  }
};

// Math and window/document fns

export function rand_int(min: number, max: number): number {
  if (max === undefined) {
    max = min;
    min = 0;
  }
  return (Math.random() * (max - min) + min) | 0;
}

export function add_scroll_callback(callback: (event: WheelEvent) => void) {
  document.addEventListener('wheel', callback, {passive: true}) 
}

export function event_xy(event: TouchEvent): [number, number] {
  return [event.touches[0].clientX / window.innerWidth, event.touches[0].clientY / window.innerHeight]
}

export function add_touchstart_callback(target: HTMLElement, callback: TouchCallback) {
  target.addEventListener("touchstart", callback, {passive: false})
}

export function add_touch_callbacks(target: HTMLElement, touchstart_callback: TouchCallback, touchmove_callback: TouchCallback) {
  add_touchstart_callback(target, touchstart_callback);
  target.addEventListener("touchmove", touchmove_callback, {passive: false});
}

export function u8_to_vec3(color: Color): string {
  let [r, g, b] = color.map(n => n / 255);
  return `vec3(${r},${g},${b})`
}

// Functional utilities
export function partition(
  size: number,
  step: number,
  array: number[]
): number[] {
  if (size <= 0 || step <= 0) {
    throw new Error("Size and step must be greater than 0");
  }

  return array.reduce((result, _, i) => {
    if (i % step === 0) {
      const partitionSlice = array.slice(i, i + size);
      return [...result, ...partitionSlice];
    }
    return result;
  }, [] as number[]);
}

export function map<T, U, V>(array1: T[], array2: U[], callback: (item1: T, item2: U, index: number) => V): V[] {
  const minLength = Math.min(array1.length, array2.length);

  return Array.from({ length: minLength }, (_, index) => callback(array1[index], array2[index], index));
}

export function repeat<T>(n: number, expr: T): T[] {
  return Array(n).fill(structuredClone(expr))
}

export function comp<T1, T2, T3>(f: (x: T2) => T3, g: (x: T1) => T2): (x: T1) => T3 {
  return (x: T1) => f(g(x))
}

export { check_context };
export type { MaybeContext };
