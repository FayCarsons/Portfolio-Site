import {
  AttachmentOptions,
  BufferInfo,
  FramebufferInfo,
  ProgramInfo,
  VertexArrayInfo,
  bindFramebufferInfo,
  createBufferInfoFromArrays,
  createFramebufferInfo,
  createProgramInfo,
  drawBufferInfo,
  resizeFramebufferInfo,
  setBuffersAndAttributes,
  setUniforms,
} from "twgl.js";
import { Context } from "./context";

export enum ShaderType {
  Vertex,
  Purefrag,
}

export type Uniforms = { [key: string]: any };

interface Target {
  framebuffer: FramebufferInfo;
  format?: AttachmentOptions[];
}

interface ShaderConstructor {
  sources: { frag: ShaderSource; vert?: ShaderSource };
  type?: ShaderType;
  target?: Target;
  data?: ShaderData;
  uniforms?: Uniforms;
}

type ShaderData = { geometry_buffer: BufferInfo | VertexArrayInfo };

type ShaderSource = { glsl: string; label: string };

class Shader {
  public sources: { frag: ShaderSource; vert: ShaderSource };
  private program: ProgramInfo;
  private data: ShaderData;
  private target?: Target;
  // @ts-ignore
  private type: ShaderType;
  public uniforms: Uniforms;

  constructor(
    ctx: Context,
    { sources, target, type, data, uniforms }: ShaderConstructor
  ) {
    this.sources = { frag: sources.frag, vert: sources.vert || trivial_vert };
    if (!type || !data || type === ShaderType.Purefrag) {
      const fullscreen_triangle_buffer = {
        geometry_buffer: createBufferInfoFromArrays(ctx.gl, {
          position: { numComponents: 2, data: [-1, 3, -1, -1, 3, -1] },
        }),
      };
      this.data = { ...fullscreen_triangle_buffer, ...data };
    } else {
      this.data = data;
    }
    this.type = type || ShaderType.Purefrag;
    this.uniforms = uniforms || { size: ctx.resolution };
    this.target = target;
    this.program = createProgramInfo(ctx.gl, this.program_pair);

    // PLACEHOLDER
    this.type = ShaderType.Purefrag;
  }

  add_target({ gl }: Context, opts: AttachmentOptions) {
    const attachments =
      opts instanceof Array ? opts : opts ? [opts] : undefined;
    const framebuffer = createFramebufferInfo(gl, attachments);
    this.target = { framebuffer, format: attachments };
  }

  use(gl: WebGL2RenderingContext) {
    gl.useProgram(this.program.program);
    setBuffersAndAttributes(gl, this.program, this.data.geometry_buffer);
  }

  reset_uniforms(uniforms: Uniforms) {
    this.uniforms = uniforms;
  }

  update_uniforms(update: (uniforms: Uniforms) => Uniforms) {
    this.uniforms = update(this.uniforms);
  }

  bind_uniforms() {
    setUniforms(this.program, this.uniforms);
  }

  render(ctx: Context, size?: [number, number]) {
    const { gl } = ctx;
    const resolution = size || ctx.resolution; 
    this.use(gl);
    this.bind_uniforms();
    setBuffersAndAttributes(gl, this.program, this.data.geometry_buffer);
    if (this.target) {
      resizeFramebufferInfo(
        gl,
        this.target.framebuffer,
        this.target.format,
        resolution[0],
        resolution[1]
      );
    }
    bindFramebufferInfo(gl, this.target?.framebuffer);
    drawBufferInfo(gl, this.data.geometry_buffer);
  }

  get program_pair(): [string, string] {
    return [this.sources.vert.glsl, this.sources.frag.glsl];
  }

  get textures(): (WebGLTexture | WebGLRenderbuffer)[] | null {
    if (!this.target) {
      throw new Error(
        `Attempting to access non-existant textures in ${this.sources.frag.label}`
      );
    }
    const textures = this.target.framebuffer.attachments;
    return textures instanceof Array ? textures : [textures];
  }
}

const TRIVIAL_VERT_GLSL = `#version 300 es
in vec2 position;
void main() {
    gl_Position = vec4(position, 0.0, 1.0);
}`;

const trivial_vert = {
  glsl: TRIVIAL_VERT_GLSL,
  label: "fullscreen triangle vertex shader",
};

export { Shader };
