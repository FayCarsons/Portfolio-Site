import type { Shader } from "../shaders/Shader";
import fragment from './Lambda.glsl?raw'

export default function Lambda(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { title: "Lambda", fragment, canvas }
}