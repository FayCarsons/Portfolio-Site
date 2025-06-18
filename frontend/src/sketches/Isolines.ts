import { Shader } from "../shaders/Shader";
import fragment from "./Isolines.glsl?raw"

export default function Isolines(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { title: "Isolines", canvas, fragment }
}
