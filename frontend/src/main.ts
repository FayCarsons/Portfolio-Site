import { GraphViz } from "./graph"
import { loadBlogPreviews } from "./blog";
import { Shader } from "./shaders/Shader";
import { Isolines } from "./sketches/Isolines";
import { Triangles } from "./sketches/Triangles";

// Initialize when DOM is ready
document.addEventListener('DOMContentLoaded', loadBlogPreviews);

// Fixed parameters for the FDG class
const graphContainer = document.getElementById('widget1')!
const bounds = graphContainer.getBoundingClientRect()

const graph = new GraphViz.FDG({
  bounds,
  repulsion: 800,          // Strong repulsion for good spacing
  attraction: 0.08,         // Balanced spring force
  centerAttraction: 0.01, // Very gentle center pull
  damping: 0.5,           // Smooth gradual settling
  maxSpeed: 1
})

const renderer = new GraphViz.Renderer(graphContainer)
const nNodes = 16

// Add nodes with uniform mass and better spacing
for (let i = 0; i < nNodes; i++) {
  graph.addNode(
    Math.random() * bounds.width * 0.6 + bounds.width * 0.2,  // More centered distribution
    Math.random() * bounds.height * 0.6 + bounds.height * 0.2,
    1.0  // Uniform mass for predictable behavior
  )
}

// Fixed edge generation - ensure connectivity and proper indices
// Create spanning tree with consistent edge lengths
for (let i = 1; i < nNodes; i++) {
  const target = Math.floor(Math.random() * i);
  graph.addEdge(i, target, 80);  // Consistent edge length
}

// Add fewer, more intentional extra edges
const extraEdges = Math.floor(nNodes * 0.3);  // Only 30% extra edges
for (let i = 0; i < extraEdges; i++) {
  const source = Math.floor(Math.random() * nNodes);
  const target = Math.floor(Math.random() * nNodes);

  if (source !== target) {
    graph.addEdge(source, target, 80);  // Consistent edge length
  }
}

const isolines = document.getElementById('isolines')!
const isolinesShader = Shader.make(Isolines(isolines as HTMLCanvasElement))

const triangles = document.getElementById('triangles')!
console.log(triangles)
const trianglesShader = Shader.make(Triangles(triangles as HTMLCanvasElement))

// Animation loop
function animate() {
  graph.tick()
  renderer.render(graph)
  isolinesShader?.render()
  trianglesShader?.render()
  requestAnimationFrame(animate)
}

animate()