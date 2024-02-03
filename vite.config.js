import { defineConfig } from "vite";
import solid from "vite-plugin-solid";
import tailwind from 'tailwindcss'

export default defineConfig({
  plugins: [solid()],
  css: {
    postcss: "./postcss.config.js",
  },
});