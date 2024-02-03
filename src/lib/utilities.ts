type TouchCallback = (event: TouchEvent) => void;

// EVENT HANDLER FNS
export function add_scroll_callback(callback: (event: WheelEvent) => void) {
  document.addEventListener("wheel", callback, { passive: true });
}

export const event_xy = (event: TouchEvent): [number, number] => {
  return [
    event.touches[0].clientX / window.innerWidth,
    event.touches[0].clientY / window.innerHeight,
  ];
};

export const add_touchstart_callback = (
  target: HTMLElement,
  callback: TouchCallback
) => {
  target.addEventListener("touchstart", callback, { passive: false });
};

export const add_touch_callbacks = (
  target: HTMLElement,
  touchstart_callback: TouchCallback,
  touchmove_callback: TouchCallback
) => {
  add_touchstart_callback(target, touchstart_callback);
  target.addEventListener("touchmove", touchmove_callback, { passive: false });
};

// ???

export const log = <T>(x: T): T => {
  console.log(x);
  return x;
};

// MATH FNS
export const rand_int = (min: number, max: number): number => {
  if (max === undefined) {
    max = min;
    min = 0;
  }
  return (Math.random() * (max - min) + min) | 0;
};

export const rand_nth = <T>(a: T[]): T => {
  return a[rand_int(0, a.length)];
};

export const uint_to_vec3 = (...args: number[]): string => {
  let [rf, gf, bf] = args.map((n) => n / 255);
  return `vec3(${rf},${gf},${bf})`;
};

// Functional utilities
export const partition = (
  size: number,
  step: number,
  array: number[]
): number[] => {
  if (size <= 0 || step <= 0) {
    throw new Error("Size and step must be greater than 0");
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
  return Array.from({length: end - start}, (_, idx) => start + idx)
}

export const map2 = <T, U, V>(
  array1: T[],
  array2: U[],
  callback: (item1: T, item2: U, index: number) => V
): V[] => {
  const min_len = Math.min(array1.length, array2.length);
  return Array.from({ length: min_len }, (_, index) =>
    callback(array1[index], array2[index], index)
  );
};

export const repeat = <T>(n: number, expr: T): T[] => {
  return Array(n)
    .fill(0)
    .map(() => structuredClone(expr));
};

export const comp = <T, U, V>(
  f: (x: U) => V,
  g: (x: T) => U
): ((x: T) => V) => {
  return (x: T) => f(g(x));
};

export const take = <T>(n: number, coll: T[]): T[] => {
  return coll.slice(0, n);
};

export const drop = <T>(n: number, coll: T[]): T[] => {
  return coll.slice(n);
};

// RESULTS AND OPTIONS

interface ResBlock<T, E, U, V> {
  ok: (v: T) => U;
  err: (e: E) => V;
}

export class Result<T, E = Error | string> {
  inner: T | E;
  ok: boolean;

  constructor(inner: T | E) {
    this.inner = inner;
    this.ok = !(inner instanceof Error);
  }

  static ok<U, V>(inner: U): Result<U, V> {
    return new Result<U, V>(inner);
  }

  static err<U, V>(err: V): Result<U, V> {
    return new Result<U, V>(err);
  }

  get is_ok(): boolean {
    return this.ok;
  }

  map(): Option<T | null> {
    if (this.ok) {
      return Option.some(this.inner as T);
    } else {
      return Option.none();
    }
  }

  flatmap<U>(f: (val: T) => U): Result<U | T, E> | null {
    if (this.ok) {
      return Result.ok(f(this.inner as T));
    } else {
      return this;
    }
  }

  unwrap<A>(): A {
    if (!this.ok) throw this.inner;
    return this.inner as A;
  }

  match<U, V>({ ok, err }: ResBlock<T, E, U, V>) {
    if (this.ok) {
      return ok(this.inner as T);
    } else {
      return err(this.inner as E);
    }
  }
}

interface OptBlock<T, U> {
  some: (val: T) => U;
  none: () => U;
}
export class Option<T> {
  inner: T | null;
  some: boolean;

  constructor(inner: T | null | undefined) {
    this.inner = inner || null;
    this.some = !!inner;
  }

  static some<U>(inner: U): Option<U> {
    return new Option(inner);
  }

  static none<T>(): Option<T> {
    return new Option(null) as Option<T>;
  }

  is_some(): boolean {
    return this.some;
  }

  map<E = Error | string>(err: E): Result<T, E> {
    if (this.some) {
      return new Result<T, E>(this.inner as T);
    } else {
      return new Result<T, E>(err);
    }
  }

  and_then<U>(f: (inner: T) => U): Option<U | T> {
    if (this.some) {
      return Option.some(f(this.inner as T));
    } else {
      return this;
    }
  }

  unwrap(): T | null {
    try {
      if (this.some) {
        return this.inner as T;
      } else {
        throw new Error("Unwrapped on a 'None' value");
      }
    } catch (e) {
      console.error(e);
      return null;
    }
  }

  match<U>({ some, none }: OptBlock<T, U>) {
    if (this.some) {
      return some(this.inner as T);
    } else {
      return none();
    }
  }
}
