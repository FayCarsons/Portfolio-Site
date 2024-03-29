import { bindFramebufferInfo } from 'twgl.js';
import { Shader } from './shader';

export const createContext = (
  canvas: HTMLCanvasElement,
): WebGL2RenderingContext => {
  const gl = canvas.getContext('webgl2');

  if (!gl) {
    throw new Error('WebGL 2 not available on this device');
  }

  gl.clearColor(255, 255, 255, 255);
  return gl;
};

export const maximizeCanvas = (gl: WebGL2RenderingContext) => {
  gl.canvas.width = window.innerWidth;
  gl.canvas.height = window.innerHeight;
};

export const setTarget = (gl: WebGL2RenderingContext, shader: Shader) => {
  bindFramebufferInfo(gl, shader.target?.framebuffer);
};

export const canvasResolution = (
  gl: WebGL2RenderingContext,
): [number, number] => {
  return [gl.canvas.width, gl.canvas.height];
};
