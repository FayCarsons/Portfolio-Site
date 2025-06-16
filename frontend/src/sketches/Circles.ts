import { Shader } from "../shaders/Shader";
import fragment from './Circles.glsl?raw'

export default function Circles(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { canvas, fragment }
}