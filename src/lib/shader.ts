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
} from 'twgl.js';
import { canvasResolution } from './context';

export type Uniforms = { [key: string]: number | number[] | number[][] };

interface Target {
  framebuffer: FramebufferInfo;
  format?: AttachmentOptions[];
}

interface ShaderConstructor {
  sources: { frag: ShaderSource; vert?: ShaderSource };
  target?: Target;
  data?: ShaderData;
  uniforms?: Uniforms;
}
type ShaderData = { geometry_buffer: BufferInfo | VertexArrayInfo };
type ShaderSource = { glsl: string; label: string };

export type Shader = {
  sources: { frag: ShaderSource; vert: ShaderSource };
  program: ProgramInfo;
  data: ShaderData;
  target?: Target;
  uniforms: Uniforms;
};

export const createShader = (
  gl: WebGL2RenderingContext,
  args: ShaderConstructor,
): Shader => {
  const { sources, target, data, uniforms } = args;

  const is_purefrag = sources.vert === null;

  const instance_sources = {
    frag: sources.frag,
    vert: sources.vert || trivialVert,
  };

  let instance_data;
  if (is_purefrag || !data) {
    const fullscreen_triangle_buffer = {
      geometry_buffer: createBufferInfoFromArrays(gl, {
        position: { numComponents: 2, data: [-1, 3, -1, -1, 3, -1] },
      }),
    };
    instance_data = { ...fullscreen_triangle_buffer, ...data };
  } else {
    if (!data) throw new Error('Vert shaders requires vertex data');
    instance_data = data;
  }
  const instance_uniforms = uniforms || { size: canvasResolution(gl) };

  const program_pair = [instance_sources.vert.glsl, instance_sources.frag.glsl];
  const program = createProgramInfo(gl, program_pair);

  return {
    sources: instance_sources,
    program,
    target,
    data: instance_data,
    uniforms: instance_uniforms,
  };
};

export const addTarget = (
  gl: WebGL2RenderingContext,
  shader: Shader,
  opts: AttachmentOptions,
): Shader => {
  const attachments = Array.isArray(opts) ? opts : [opts];
  const framebuffer = createFramebufferInfo(gl, attachments);
  const target: Target = { framebuffer, format: attachments };
  return { ...shader, target };
};

export const useShader = (gl: WebGL2RenderingContext, shader: Shader) => {
  gl.useProgram(shader.program.program);
  setBuffersAndAttributes(gl, shader.program, shader.data.geometry_buffer);
};

export const resetUniforms = (shader: Shader, uniforms: Uniforms): Shader => {
  return { ...shader, uniforms };
};

export const updateUniforms = (
  shader: Shader,
  update: (uniforms: Uniforms) => Uniforms,
): Shader => {
  const updated_uniforms = update(shader.uniforms);
  return { ...shader, uniforms: updated_uniforms };
};

export const bindUniforms = (shader: Shader) => {
  setUniforms(shader.program, shader.uniforms);
};

export const runShader = (
  gl: WebGL2RenderingContext,
  shader: Shader,
  size?: [number, number],
) => {
  const resolution = size || canvasResolution(gl);
  useShader(gl, shader);
  bindUniforms(shader);
  if (shader.target) {
    resizeFramebufferInfo(
      gl,
      shader.target.framebuffer,
      shader.target.format,
      resolution[0],
      resolution[1],
    );
  }
  bindFramebufferInfo(gl, shader.target?.framebuffer);
  drawBufferInfo(gl, shader.data.geometry_buffer);
};

export const getTextures = (shader: Shader) => {
  if (!shader.target)
    throw new Error(
      `Cannot access non-existant texs in ${shader.sources.frag.label}`,
    );
  const textures = shader.target.framebuffer.attachments;
  return Array.isArray(textures) ? textures : [textures];
};

const TRIVIAL_VERT_GLSL = `#version 300 es
in vec2 position;
void main() {
    gl_Position = vec4(position, 0.0, 1.0);
}`;

const trivialVert = {
  glsl: TRIVIAL_VERT_GLSL,
  label: 'fullscreen triangle vertex shader',
};
