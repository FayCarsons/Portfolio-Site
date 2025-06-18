import type { Shader } from "../shaders/Shader";
import fragment from './Target.glsl?raw'

export default function Target(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { title: "Target", canvas, fragment }
}