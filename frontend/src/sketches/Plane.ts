import type { Shader } from "../shaders/Shader";
import fragment from './Plane.glsl?raw'

export default function Plane(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { canvas, fragment }
}