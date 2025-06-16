import type { Shader } from '../shaders/Shader';
import fragment from './Osc.glsl?raw'

export default function Osc(canvas: HTMLCanvasElement): Shader.ShaderDescriptor {
    return { canvas, fragment }
}