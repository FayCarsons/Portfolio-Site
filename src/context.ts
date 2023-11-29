import { bindFramebufferInfo } from "twgl.js";

export class Context {
    public gl: WebGL2RenderingContext;
    constructor(
      canvas: HTMLCanvasElement
    ) {
      this.gl = canvas.getContext("webgl2") as WebGL2RenderingContext;
      this.gl.clearColor(255, 255, 255, 255);
      this.init();
    }
  
    init() {
      this.gl.getExtension('OES_texture_float');
    }
  
    maximize_canvas() {
      let [width, height] = [window.innerWidth, window.innerHeight];
      this.gl.canvas.width = width;
      this.gl.canvas.height = height;
    }
  
    target_screen() {
      bindFramebufferInfo(this.gl, null)
    }
  
    get resolution(): [number, number] {
      return [this.gl.canvas.width, this.gl.canvas.height];
    }
  }