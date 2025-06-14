import { Shader } from "../shaders/Shader";
import fragment from "./Isolines.glsl?raw"

export function Isolines(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { canvas, fragment }
}
