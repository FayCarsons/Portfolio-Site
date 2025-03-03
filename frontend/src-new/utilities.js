// EVENT HANDLER FNS
export function addScrollCallback(callback) {
  document.addEventListener('wheel', callback, { passive: true })
}

export function removeScrollCallback(callback) {
  document.removeEventListener('wheel', callback)
}

export function eventXY(event) {
  return [
    event.touches[0].clientX / window.innerWidth,
    event.touches[0].clientY / window.innerHeight,
  ]
}

export function addTouchCallbacks(target, touchstart, touchmove) {
  target.addEventListener('touchstart', touchstart, { passive: false })
  target.addEventListener('touchmove', touchmove, { passive: false });
}

export function removeTouchCallbacks(target, touchstartCallback, touchmoveCallback) {
  target.removeEventListener('touchstart', touchstartCallback);
  target.removeEventListener('touchmove', touchmoveCallback);
};

export function log(x) {
  console.log(x)
  return x
}

// MATH FNS
export function randInt(min, max) {
  if (max == null || max == undefined) {
    max = min
    min = 0
  }

  return (Math.random() * (max - min) + min) | 0
}

export function randNth(a) {
  return a[randInt(0, a.length)]
}

export function uintToVec3(...args) {
  if (args.length < 3)
    throw new Error("Vec3 needs 3 elements")
  const [r, g, b] = args.map(n => n / 255.)
  return `vec3(${r}, ${g}, ${b})`
}

// Functional utilities
export function windows(size, step, arr) {
  if (size <= 0 || step <= 0)
    throw new Error('windows: size and step must be greater than zero')

  arr.reduce((result, i) => {
    if (i % step === 0) {
      const window = arr.slice(i, i + size)
      return [...result, ...window]
    }

    return result
  }, [])
}

export function partition(f, arr) {
  const res = [[], []]
  arr.forEach(e => {
    res[Number(f(e))] = e
  });
  res
}

export function range(start, end) {
  return Array.from({ length: end - start }, (_, idx) => start + idx)
}

export function map2(a, b, f) {
  return Array.from({ length: Math.min(a.length, b.length) }, (_, idx) => f(a[idx], b[idx]))
}

export function always(x) {
  return _ => x
}

export function repeat(n, x) {
  return Array(n).fill(0).map(always(x))
}

export function comp(f, g) {
  return x => f(g(x))
}

export function take(n, coll) {
  return coll.slice(0, n)
}
export function drop(n, coll) {
  return coll.slice(n)
}

export const inc = n => n + 1;
export const dec = n => n - 1;
