type TouchCallback = (event: TouchEvent) => void;

// EVENT HANDLER FNS
export function addScrollCallback(callback: (event: WheelEvent) => void) {
  document.addEventListener('wheel', callback, { passive: true });
}

export const event_xy = (event: TouchEvent): [number, number] => {
  return [
    event.touches[0].clientX / window.innerWidth,
    event.touches[0].clientY / window.innerHeight,
  ];
};

export const addTouchstartCallback = (
  target: HTMLElement,
  callback: TouchCallback,
) => {
  target.addEventListener('touchstart', callback, { passive: false });
};

export const addTouchCallbacks = (
  target: HTMLElement,
  touchstart_callback: TouchCallback,
  touchmove_callback: TouchCallback,
) => {
  addTouchstartCallback(target, touchstart_callback);
  target.addEventListener('touchmove', touchmove_callback, { passive: false });
};

// ???
export const log = <T>(x: T): T => {
  console.log(x);
  return x;
};

// MATH FNS
export const randInt = (min: number, max: number): number => {
  if (max === undefined) {
    max = min;
    min = 0;
  }
  return (Math.random() * (max - min) + min) | 0;
};

export const randNth = <T>(a: T[]): T => {
  return a[randInt(0, a.length)];
};

export const uintToVec3 = (...args: number[]): string => {
  let [rf, gf, bf] = args.map((n) => n / 255);
  return `vec3(${rf},${gf},${bf})`;
};

// Functional utilities
export const partition = (
  size: number,
  step: number,
  array: number[],
): number[] => {
  if (size <= 0 || step <= 0) {
    throw new Error('Size and step must be greater than 0');
  }

  return array.reduce((result, _, i) => {
    if (i % step === 0) {
      const partition_slice = array.slice(i, i + size);
      return [...result, ...partition_slice];
    }
    return result;
  }, [] as number[]);
};

export const range = (start: number, end: number) => {
  return Array.from({ length: end - start }, (_, idx) => start + idx);
};

export const map2 = <T, U, V>(
  array1: T[],
  array2: U[],
  callback: (item1: T, item2: U, index: number) => V,
): V[] => {
  const min_len = Math.min(array1.length, array2.length);
  return Array.from({ length: min_len }, (_, index) =>
    callback(array1[index], array2[index], index),
  );
};

export const repeat = <T>(n: number, expr: T): T[] => {
  return Array(n)
    .fill(0)
    .map(() => structuredClone(expr));
};

export const comp = <T, U, V>(
  f: (x: U) => V,
  g: (x: T) => U,
): ((x: T) => V) => {
  return (x: T) => f(g(x));
};

export const take = <T>(n: number, coll: T[]): T[] => {
  return coll.slice(0, n);
};

export const drop = <T>(n: number, coll: T[]): T[] => {
  return coll.slice(n);
};
