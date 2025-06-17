import { defineConfig } from 'vite'
import { resolve } from 'path'
import { globSync } from 'glob'
import fs from 'fs'

export default defineConfig({
    base: '/',
    build: {
        outDir: 'dist',
        rollupOptions: {
            input: {
                main: resolve(__dirname, 'index.html'),
                ...Object.fromEntries(
                    [
                        ...globSync('*.html'),           // Root level pages
                        ...globSync('articles/**/*.html'), // Articles
                        ...globSync('tags/**/*.html')      // Tags
                    ]
                        .filter(file => file !== 'index.html') // Don't duplicate index
                        .map(file => [
                            file.slice(0, -5),
                            resolve(__dirname, file)
                        ])
                )
            }
        }
    }
})