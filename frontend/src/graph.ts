export namespace GraphViz {
    interface Node {
        x: number;
        y: number;
        vx: number; // velocity x
        vy: number; // velocity y
        mass: number;
    }

    interface Edge {
        source: number;
        target: number;
        length: number; // desired spring length
    }

    interface GraphParams {
        repulsion: number;       // how much nodes push apart
        attraction: number;      // spring strength
        centerAttraction: number; // pull toward center
        damping: number;         // friction (0-1)
        maxSpeed: number;        // terminal velocity
        bounds?: { width: number; height: number };
    }

    export class FDG {
        nodes: Array<Node> = [];
        edges: Edge[] = [];
        options: GraphParams;

        constructor(options: Partial<GraphParams> = {}) {
            this.options = {
                repulsion: 300,           // Reduced for less aggressive pushing
                attraction: 0.005,         // Much weaker springs
                centerAttraction: 0.03,  // Gentle pull to center
                damping: 0.0001,           // Less damping = longer to settle
                maxSpeed: 1,             // Slower max speed
                ...options
            };
        }

        addNode(x = 0, y = 0, mass = 1): void {
            this.nodes.push({
                x: x || Math.random() * 400,
                y: y || Math.random() * 400,
                vx: 0,
                vy: 0,
                mass
            });
        }

        addEdge(source: number, target: number, length = 80): void { // Longer default springs
            this.edges.push({ source, target, length });
        }

        tick(): void {
            const nodes = this.nodes;

            // Apply damping
            nodes.forEach(node => {
                node.vx *= this.options.damping;
                node.vy *= this.options.damping;
            });

            // Center attraction - pull everything gently toward center
            if (this.options.bounds) {
                const centerX = this.options.bounds.width / 2;
                const centerY = this.options.bounds.height / 2;

                nodes.forEach(node => {
                    const dx = centerX - node.x;
                    const dy = centerY - node.y;

                    node.vx += dx * this.options.centerAttraction;
                    node.vy += dy * this.options.centerAttraction;
                });
            }

            // Repulsion: nodes push each other away
            for (let i = 0; i < nodes.length; i++) {
                for (let j = i + 1; j < nodes.length; j++) {
                    const a = nodes[i];
                    const b = nodes[j];

                    const dx = b.x - a.x;
                    const dy = b.y - a.y;
                    const dist = Math.sqrt(dx * dx + dy * dy) || 0.01;

                    // Weaker repulsion that falls off faster
                    const force = this.options.repulsion / (dist * dist);
                    const fx = (dx / dist) * force;
                    const fy = (dy / dist) * force;

                    a.vx -= fx / a.mass;
                    a.vy -= fy / a.mass;
                    b.vx += fx / b.mass;
                    b.vy += fy / b.mass;
                }
            }

            // Spring attraction: edges act like springs
            this.edges.forEach(edge => {
                const source = this.nodes[edge.source];
                const target = this.nodes[edge.target];

                if (!source || !target) return;

                const dx = target.x - source.x;
                const dy = target.y - source.y;
                const dist = Math.sqrt(dx * dx + dy * dy) || 0.01;

                // Much weaker springs
                const displacement = dist - edge.length;
                const force = displacement * this.options.attraction;

                const fx = (dx / dist) * force;
                const fy = (dy / dist) * force;

                source.vx += fx / source.mass;
                source.vy += fy / source.mass;
                target.vx -= fx / target.mass;
                target.vy -= fy / target.mass;
            });

            // Apply velocities with speed limiting
            nodes.forEach(node => {
                const speed = Math.sqrt(node.vx * node.vx + node.vy * node.vy);
                if (speed > this.options.maxSpeed) {
                    node.vx = (node.vx / speed) * this.options.maxSpeed;
                    node.vy = (node.vy / speed) * this.options.maxSpeed;
                }

                node.x += node.vx;
                node.y += node.vy;

                // Softer boundary constraints
                if (this.options.bounds) {
                    const { width, height } = this.options.bounds;
                    const margin = 30;

                    if (node.x < margin) node.vx += (margin - node.x) * 0.05;
                    if (node.x > width - margin) node.vx -= (node.x - (width - margin)) * 0.05;
                    if (node.y < margin) node.vy += (margin - node.y) * 0.05;
                    if (node.y > height - margin) node.vy -= (node.y - (height - margin)) * 0.05;
                }
            });
        }

        simulate(steps = 100): void {
            for (let i = 0; i < steps; i++) {
                this.tick();
            }
        }

        energy(): number {
            return this.nodes
                .reduce((sum, node) => sum + (node.vx * node.vx + node.vy * node.vy), 0);
        }

        positions(): Array<{ x: number; y: number }> {
            return this.nodes
                .map(node => ({ x: node.x, y: node.y }));
        }
    }

    // Your existing LofiForceGraph class goes here, then add this renderer:

    export class Renderer {
        private parent: HTMLElement | null
        private ctx: CanvasRenderingContext2D;
        private time = 0;

        constructor(private canvas: HTMLCanvasElement) {
            this.parent = canvas.parentElement!
            this.canvas.width = this.parent.clientWidth
            this.canvas.height = this.parent.clientHeight

            this.canvas.style.display = "block"
            this.canvas.style.margin = "0"
            this.canvas.style.padding = "0"
            this.canvas.style.background = '#ffffff';

            this.ctx = this.canvas.getContext('2d')!;
            this.resize();

            // Auto-resize
            new ResizeObserver(() => this.resize()).observe(this.parent);
        }

        public resize() {
            const { width, height } = this.parent!.getBoundingClientRect()
            this.canvas.width = width
            this.canvas.height = height
            this.canvas.style.width = "100%"
            this.canvas.style.height = "100%"
        }

        render(graph: FDG) {
            this.time += 0.01;
            const ctx = this.ctx;
            const { width, height } = this.canvas;

            // Clear with subtle motion blur
            ctx.fillStyle = 'white';
            ctx.fillRect(0, 0, width, height);

            // Render edges first (so they appear behind nodes)
            graph.edges.forEach((edge: Edge) => {
                const source = graph.nodes[edge.source]
                const target = graph.nodes[edge.target]
                if (!(source && target)) return

                const x1 = source.x;
                const y1 = source.y;
                const x2 = target.x;
                const y2 = target.y;

                // Main edge
                ctx.beginPath();
                ctx.moveTo(x1, y1);
                ctx.lineTo(x2, y2);
                ctx.strokeStyle = '#cccccc'; // Lighter gray so it's more visible
                ctx.lineWidth = 1.5;
                ctx.stroke();
            });

            // Render nodes (smaller and cleaner)
            graph.nodes.forEach(node => {
                if (!node) return; // Handle sparse arrays

                const x = node.x;
                const y = node.y;
                const nodeRadius = 4; // Much smaller

                // Subtle node shadow
                ctx.beginPath();
                ctx.arc(x + 0.5, y + 0.5, nodeRadius, 0, Math.PI * 2);
                ctx.fillStyle = 'rgba(0, 0, 0, 0.1)';
                ctx.fill();

                // Main node
                ctx.beginPath();
                ctx.arc(x, y, nodeRadius, 0, Math.PI * 2);
                ctx.fillStyle = '#888888';
                ctx.fill();

                // Node border
                ctx.beginPath();
                ctx.arc(x, y, nodeRadius, 0, Math.PI * 2);
                ctx.strokeStyle = '#aaaaaa';
                ctx.lineWidth = 0.5;
                ctx.stroke();
            });
        }
    }



    export function make(canvas: HTMLCanvasElement): { render: () => void, resize: () => void, isCanvas: true } {
        const { width, height } = canvas;
        const bounds = { width, height }


        const graph = new GraphViz.FDG({
            bounds,
            repulsion: 800,          // Strong repulsion for good spacing
            attraction: 0.08,         // Balanced spring force
            centerAttraction: 0.01, // Very gentle center pull
            damping: 0.5,           // Smooth gradual settling
            maxSpeed: 1
        })

        const renderer = new GraphViz.Renderer(canvas)
        const nNodes = 18

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

        return {
            ...renderer,
            isCanvas: true,
            resize: renderer.resize,
            render() {
                graph.tick()
                renderer.render(graph)
            }
        }
    }
}