import { Builtins } from "./Builtins"
import * as TWGL from 'twgl.js'
import trivialVert from './TrivialVert.glsl?raw'

export namespace Shader {
    export type UniformFactory = (gl: WebGL2RenderingContext) => void

    export interface Shader {
        gl: WebGL2RenderingContext
        builtins: Builtins.Builtins
        render: () => void
        resize: () => void
    }

    export interface ShaderDescriptor<T = {}> {
        canvas: HTMLCanvasElement
        fragment: string
        uniforms?: T
        init?: UniformFactory
        pre?: (gl: WebGL2RenderingContext) => void
        post?: (gl: WebGL2RenderingContext) => void
        resize?: (gl: WebGL2RenderingContext) => void
    }

    export function make<T = {}>({ canvas, fragment, uniforms, init, pre, post, resize }: ShaderDescriptor<T>): Shader | undefined {
        const parent = canvas.parentElement!
        canvas.width = parent.clientWidth
        canvas.height = parent.clientHeight

        const gl = canvas.getContext('webgl2')
        if (!gl) {
            alert('WebGL is not supported in your browser, animations are disabled')
            return
        }

        const builtins = Builtins.make()
        init?.(gl!)

        const programInfo = TWGL.createProgramInfo(gl!, [trivialVert, fragment])
        if (!programInfo) {
            console.error(`Shader failed to compile`)
        }

        const vao = gl.createVertexArray()
        gl.bindVertexArray(vao)

        function onResize() {
            TWGL.resizeCanvasToDisplaySize(canvas)
            gl?.viewport(0, 0, canvas.width, canvas.height)
            builtins.size = [canvas.width, canvas.height]
            resize?.(gl!)
        }

        function render() {
            builtins.tick()
            pre?.(gl!)
            gl?.useProgram(programInfo.program)
            TWGL.setUniforms(programInfo, { ...builtins, ...(uniforms ?? {}) })
            gl?.drawArrays(gl.TRIANGLES, 0, 3)
            post?.(gl!)
        }

        return {
            gl,
            builtins,
            render,
            resize: onResize
        }
    }
}
