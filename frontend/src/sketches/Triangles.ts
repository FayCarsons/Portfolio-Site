import type { Shader } from "../shaders/Shader";
import fragment from './Triangles.glsl?raw'

export default function Triangles(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { title: "Triangles", canvas, fragment }
}