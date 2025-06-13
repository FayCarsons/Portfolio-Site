// Add interactive elements
const shaderBoxes = document.querySelectorAll('.shader-box');

shaderBoxes.forEach(box => {
    box.addEventListener('mouseenter', () => {
        box.style.transform = 'scale(1.02)';
    });
    
    box.addEventListener('mouseleave', () => {
        box.style.transform = 'scale(1)';
    });
});

// Matrix rain effect
const matrixElements = document.querySelectorAll('.shader-matrix');
matrixElements.forEach(matrix => {
    setInterval(() => {
        const chars = '01';
        let content = '';
        for (let i = 0; i < 6; i++) {
            for (let j = 0; j < 8; j++) {
                content += Math.random() > 0.7 ? chars[Math.floor(Math.random() * chars.length)] : matrix.textContent[i * 9 + j] || '0';
            }
            if (i < 5) content += '<br>';
        }
        matrix.innerHTML = content;
    }, 150);
});
