import { Shader } from "../shaders/Shader";
import * as TWGL from 'twgl.js'
import feedbackFragment from './Feedback.glsl?raw'
import displayFragment from './Display.glsl?raw'

type FeedbackState = {
    front: TWGL.FramebufferInfo
    back: TWGL.FramebufferInfo
    swap: () => void
    createTexs: (gl: WebGL2RenderingContext) => void
}

type FeedbackUniforms = {
    tex: WebGLTexture
}

type DisplayUniforms = {
    tex: WebGLTexture,
    Scale: 1
}

export default function Feedback(canvas: HTMLCanvasElement): [Shader.ShaderDescriptor<FeedbackUniforms>, Shader.ShaderDescriptor<DisplayUniforms>] {
    // Shared state between feedback and display shaders
    const state: Partial<FeedbackState> = {}

    function createTexs(gl: WebGL2RenderingContext) {
        // Delete framebuffers if they exist
        if (state.front) {
            gl.deleteFramebuffer(state.front.framebuffer!)
        }
        if (state.back) {
            gl.deleteFramebuffer(state.back.framebuffer!)
        }

        const desc = [{
            width: canvas.width,
            height: canvas.height,
            format: gl.RGBA_INTEGER,
            internalFormat: gl.RGBA32UI,
            type: gl.UNSIGNED_INT,
            mag: gl.NEAREST,
            min: gl.NEAREST,
            wrap: gl.CLAMP_TO_EDGE,
        }]

        state.front = TWGL.createFramebufferInfo(gl, desc, canvas.width, canvas.height)
        state.back = TWGL.createFramebufferInfo(gl, desc, canvas.width, canvas.height)

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
    const feedbackShader: Shader.ShaderDescriptor<FeedbackUniforms> = {
        canvas,
        fragment: feedbackFragment,
        uniforms: {
            tex: null as any, // Will be set in pre()
        },

        init: (gl: WebGL2RenderingContext) => {
            createTexs(gl)
        },

        pre: (gl: WebGL2RenderingContext) => {
            // Render to back buffer, read from front buffer
            TWGL.bindFramebufferInfo(gl, state.back)
            gl.viewport(0, 0, canvas.width, canvas.height)

            // Set texture to read from front buffer
            feedbackShader.uniforms!.tex = state.front!.attachments[0]
        },

        post: (_: WebGL2RenderingContext) => {
            state.swap!()
        },

        resize: (gl: WebGL2RenderingContext) => {
            createTexs(gl)

            feedbackShader.uniforms!.tex = state.front!.attachments[0]
        }
    }

    // DISPLAY SHADER - renders feedback result to canvas
    const displayShader: Shader.ShaderDescriptor<DisplayUniforms> = {
        canvas,
        fragment: displayFragment,
        uniforms: {
            tex: null as any,
            Scale: 1
        },

        pre: (gl: WebGL2RenderingContext) => {
            // Render to canvas (default framebuffer)
            TWGL.bindFramebufferInfo(gl, null)
            gl.viewport(0, 0, canvas.width, canvas.height)

            // Display the front buffer (latest feedback result)
            displayShader.uniforms!.tex = state.front!.attachments[0]
        }
    }

    return [feedbackShader, displayShader]
}
/*
// Usage example:
export function createFeedbackEffect(canvas: HTMLCanvasElement): Shader.Shader[] {
    const [feedbackDesc, displayDesc] = createFeedbackSystem(canvas)

    const feedbackShader = Shader.make(feedbackDesc)
    const displayShader = Shader.make(displayDesc)

    if (!feedbackShader || !displayShader) {
        throw new Error('Failed to create shaders')
    }

    return [feedbackShader, displayShader]
}*/