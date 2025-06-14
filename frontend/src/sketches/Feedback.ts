import { Shader } from "../shaders/Shader";
import * as TWGL from 'twgl.js'
import fragment from './Feedback.glsl?raw'

type State = {
    front: TWGL.FramebufferInfo
    back: TWGL.FramebufferInfo
    uniforms: { tex: WebGLTexture }
    swap: () => void
    createTexs: (gl: WebGL2RenderingContext) => void
}

type Uniforms = {
    tex: WebGLTexture
}

export function Feedback(canvas: HTMLCanvasElement): Shader.ShaderDescriptor<Uniforms> {
    const self: Partial<Shader.ShaderDescriptor<Uniforms> & State> = {}

    self.createTexs = function (gl: WebGL2RenderingContext) {
        // Clean up existing framebuffers if they exist
        if (self.front) {
            gl.deleteFramebuffer(self.front.framebuffer!)
            gl.deleteTexture(self.front.attachments[0] as WebGLTexture)
        }
        if (self.back) {
            gl.deleteFramebuffer(self.back.framebuffer!)
            gl.deleteTexture(self.back.attachments[0] as WebGLTexture)
        }

        const desc = {
            width: canvas.width,
            height: canvas.height,
            format: gl.RGBA,
            type: gl.UNSIGNED_BYTE,
            mag: gl.NEAREST,
            min: gl.NEAREST,
            wrap: gl.CLAMP_TO_EDGE
        }

        self.front = TWGL.createFramebufferInfo(gl, [desc])
        self.back = TWGL.createFramebufferInfo(gl, [desc])

        if (!(self.front && self.back)) throw new Error('Cannot create framebuffers')

        // Clear both buffers
        TWGL.bindFramebufferInfo(gl, self.front)
        gl.clearColor(0, 0, 0, 1)
        gl.clear(gl.COLOR_BUFFER_BIT)

        TWGL.bindFramebufferInfo(gl, self.back)
        gl.clearColor(0, 0, 0, 1)
        gl.clear(gl.COLOR_BUFFER_BIT)
    }

    function init(_: WebGL2RenderingContext) {
        self.uniforms = { tex: self.front!.attachments[0] } as Uniforms
    }

    self.init = init

    self.swap = function () {
        const temp = self.front
        self.front = self.back
        self.back = temp
    }

    self.pre = function (gl: WebGL2RenderingContext): void {
        TWGL.bindFramebufferInfo(gl, self.back)
        gl.viewport(0, 0, canvas.width, canvas.height)

        self.uniforms!.tex = self.front!.attachments[0]
    }

    self.post = function (gl: WebGL2RenderingContext): void {
        TWGL.bindFramebufferInfo(gl, null)

        gl.bindFramebuffer(gl.READ_FRAMEBUFFER, self.back?.framebuffer!)
        gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, null)
        gl.blitFramebuffer(0, 0, canvas.width, canvas.height, 0, 0, canvas.width, canvas.height, gl.COLOR_BUFFER_BIT, gl.NEAREST)

        self.swap?.()
    }

    self.resize = function (gl: WebGL2RenderingContext): void {
        self.createTexs?.(gl)
        self.uniforms!.tex = self.front!.attachments[0]
    }

    self.fragment = fragment

    return self as Shader.ShaderDescriptor<Uniforms>
}