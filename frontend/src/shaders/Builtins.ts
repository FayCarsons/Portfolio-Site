export namespace Builtins {
    export type Builtins = {
        size: [number, number],
        frame: number,
        seed: number,
        tick: () => void
    }

    export function make() {
        return {
            size: [350, 150],
            frame: 0,
            seed: Math.random() * 10_000,
            tick() {
                this.frame!++
            }
        }
    }
}