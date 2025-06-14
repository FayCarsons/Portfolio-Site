import type { Shader } from "../shaders/Shader";
import fragment from './Triangles.glsl?raw'

export function Triangles(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { canvas, fragment }
}