export namespace Builtins {
    export type Builtins = {
        size: [number, number],
        frame: number,
        tick: () => void
    }

    export const make = () => {
        const builtins: Partial<Builtins> = { size: [350, 150], frame: 0 }

        function tick() {
            builtins.frame!++
        }

        builtins.tick = tick
        return builtins as Builtins
    }
}