import { Shader } from "../shaders/Shader";
import * as TWGL from 'twgl.js'
import conwayFragment from './Conway.glsl?raw'
import display from './Display.glsl?raw'

type ConwayState = {
    front: TWGL.FramebufferInfo
    back: TWGL.FramebufferInfo
    swap: () => void
    createTexs: (gl: WebGL2RenderingContext) => void
}

type ConwayUniforms = {
    tex: WebGLTexture,
    frame: number
}

type DisplayUniforms = {
    tex: WebGLTexture,
    Scale: number
}

const Scale = 2

export default function Conway(canvas: HTMLCanvasElement): [Shader.ShaderDescriptor<ConwayUniforms>, Shader.ShaderDescriptor<DisplayUniforms>] {
    // Shared state between feedback and display shaders
    const state: Partial<ConwayState> = {}

    function createTexs(gl: WebGL2RenderingContext) {
        // Delete framebuffers if they exist
        if (state.front) {
            gl.deleteFramebuffer(state.front.framebuffer!)
        }
        if (state.back) {
            gl.deleteFramebuffer(state.back.framebuffer!)
        }

        const desc = [{
            width: canvas.width / Scale,
            height: canvas.height / Scale,
            format: gl.RGBA_INTEGER,
            internalFormat: gl.RGBA32UI,
            type: gl.UNSIGNED_INT,
            mag: gl.NEAREST,
            min: gl.NEAREST,
            wrap: gl.CLAMP_TO_EDGE,
        }]

        state.front = TWGL.createFramebufferInfo(gl, desc, canvas.width / Scale, canvas.height / Scale)
        state.back = TWGL.createFramebufferInfo(gl, desc, canvas.width / Scale, canvas.height / Scale)

        if (!(state.front && state.back)) throw new Error('Cannot create framebuffers')

        // Clear both buffers
        TWGL.bindFramebufferInfo(gl, state.front)
        gl.clearBufferuiv(gl.COLOR, 0, [4294967295, 4294967295, 4294967295, 4294967295]) // Max uint32 = white

        TWGL.bindFramebufferInfo(gl, state.back)
        gl.clearBufferuiv(gl.COLOR, 0, [4294967295, 4294967295, 4294967295, 4294967295])


        TWGL.bindFramebufferInfo(gl, null)
    }

    state.swap = function () {
        const temp = state.front
        state.front = state.back
        state.back = temp
    }

    state.createTexs = createTexs

    // FEEDBACK SHADER - renders to framebuffers
    const gameOfLife: Shader.ShaderDescriptor<ConwayUniforms> = {
        canvas,
        fragment: conwayFragment,
        uniforms: {
            tex: null as any,
            frame: 0,
        },

        init: (gl: WebGL2RenderingContext) => {
            createTexs(gl)
        },

        pre: (gl: WebGL2RenderingContext) => {
            // Render to back buffer, read from front buffer
            TWGL.bindFramebufferInfo(gl, state.back)
            gl.viewport(0, 0, canvas.width, canvas.height)

            // Set texture to read from front buffer
            gameOfLife.uniforms!.tex = state.front!.attachments[0]
        },

        post: (_: WebGL2RenderingContext) => {
            if (gameOfLife.uniforms!.frame! % 8 == 3) state.swap!()
            gameOfLife.uniforms!.frame++
        },

        resize: (gl: WebGL2RenderingContext) => {
            createTexs(gl)

            gameOfLife.uniforms!.tex = state.front!.attachments[0]
        }
    }

    // DISPLAY SHADER - renders feedback result to canvas
    const displayShader: Shader.ShaderDescriptor<DisplayUniforms> = {
        canvas,
        fragment: display,
        uniforms: {
            tex: null as any,
            Scale
        },

        pre: (gl: WebGL2RenderingContext) => {
            // Render to canvas (default framebuffer)
            TWGL.bindFramebufferInfo(gl, null)
            gl.viewport(0, 0, canvas.width, canvas.height)

            // Display the front buffer (latest feedback result)
            displayShader.uniforms!.tex = state.front!.attachments[0]
        }
    }

    return [gameOfLife, displayShader]
} 