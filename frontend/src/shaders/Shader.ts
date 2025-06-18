import { Builtins } from "./Builtins"
import * as TWGL from 'twgl.js'
import trivialVert from './TrivialVert.glsl?raw'

export namespace Shader {
    export type UniformFactory = (gl: WebGL2RenderingContext) => void

    export type Shader = {
        gl: WebGL2RenderingContext
        builtins: Builtins.Builtins
        render: () => void
        resize: () => void
    } | {
        render: () => void,
        resize: () => void,
        isCanvas: true
    }

    export interface ShaderDescriptor<T = {}> {
        title: string
        canvas: HTMLCanvasElement
        fragment: string
        uniforms?: T
        init?: UniformFactory
        pre?: (gl: WebGL2RenderingContext) => void
        post?: (gl: WebGL2RenderingContext) => void
        resize?: (gl: WebGL2RenderingContext) => void
    }

    export function make<T = {}>(desc: ShaderDescriptor<T> | { render: () => void, resize: () => void, isCanvas: true }): Shader | undefined {
        if ('isCanvas' in desc) {
            return desc as Shader
        }

        const { canvas, fragment, init, pre, post, resize, uniforms } = desc;
        const parent = canvas.parentElement ?? document.body
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
            canvas.width = parent.clientWidth
            canvas.height = parent.clientHeight
            TWGL.resizeCanvasToDisplaySize(canvas)
            gl?.viewport(0, 0, canvas.width, canvas.height)
            builtins.size = [canvas.width, canvas.height] as [number, number]
            resize?.(gl!)
        }


        const resizeObserver = new ResizeObserver(onResize)
        resizeObserver.observe(parent)

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
            builtins: builtins as Builtins.Builtins,
            render,
            resize: onResize
        }
    }

    export function toRenderer(
        initFns: Array<(canvas: HTMLCanvasElement) => ShaderDescriptor<any> | ShaderDescriptor<any>[] | { resize: () => void, render: () => void, isCanvas: true }>,
        canvases: HTMLCanvasElement[]
    ): () => void {
        const shaders = canvases
            .flatMap((canvas: HTMLCanvasElement, idx: number) => {
                const descriptor = initFns[idx](canvas)
                if ("title" in descriptor || Array.isArray(descriptor))
                    canvas.addEventListener('click', () =>
                        window.location.href = `shader.html?shader=${Array.isArray(descriptor) ? descriptor[0].title : descriptor.title}`
                    )

                return descriptor

            })
            .map(make)

        return () => {
            for (const shader of shaders) {
                shader?.render()
            }
        }
    }
}
