import { GraphViz } from "./graph"
import { Shader } from "./shaders/Shader";
import Isolines from "./sketches/Isolines";
import Triangles from "./sketches/Triangles";
import Circles from "./sketches/Circles";
import Feedback from "./sketches/Feedback";
import Plane from "./sketches/Plane";
import Target from "./sketches/Target";
import Memory from "./sketches/Memory";
import Osc from "./sketches/Osc";
import Conway from "./sketches/Conway";
import { loadBlogPreviews } from './blog'

function shuffle<T>(xs: T[]): T[] {
  const result = [...xs]
  for (let i = result.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [result[i], result[j]] = [result[j], result[i]]
  }

  return result
}

const canvases = document.querySelectorAll('.glsl')
const sketches = [Isolines,
  Triangles,
  Circles,
  Feedback,
  Plane,
  Target,
  Memory,
  Osc,
  Conway,
  GraphViz.make
]
const render = Shader.toRenderer(
  shuffle(sketches),
  Array.from(canvases) as HTMLCanvasElement[]
)

// Animation loop
function animate() {
  render()
  requestAnimationFrame(animate)
}

animate()