import type { Shader } from '../shaders/Shader'
import fragment from './Memory.glsl?raw'

export default function Memory(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { title: "Memory", fragment, canvas }
}