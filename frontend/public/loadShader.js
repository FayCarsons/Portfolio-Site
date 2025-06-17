import { Shader } from '../src/shaders/Shader.ts'

async function loadShader() {
    try {
        // Get shader name from URL
        const params = new URLSearchParams(window.location.search);
        const shaderName = params.get('shader');

        if (!shaderName) {
            throw new Error('No shader specified');
        }

        // Update title
        document.getElementById('shader-title').textContent = '';

        // Dynamic import
        const module = await import(`./src/sketches/${shaderName}.ts`);
        const shaderFunction = module.default

        if (!shaderFunction) {
            throw new Error(`Shader function '${shaderName}' not found`);
        }

        // Setup canvas
        const canvas = document.getElementById('shader-canvas');
        if (!canvas) alert(`No canvas: ${canvas}`)
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;

        const descriptor = shaderFunction(canvas)
        const shaders = Array.isArray(descriptor) ? descriptor : [descriptor]
        // Initialize shader
        const runner = shaders.map(Shader.make);

        // Hide loading, show controls
        document.getElementById('loading').style.display = 'none';

        function animate() {
            runner.forEach(x => x?.render())
            requestAnimationFrame(animate);
        }
        animate();
    } catch (error) {
        console.error('Failed to load shader:', error);
        document.getElementById('loading').style.display = 'none';
        document.getElementById('error').style.display = 'block';
    }
}

// Load shader on page load
loadShader();