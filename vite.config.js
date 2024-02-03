import { defineConfig } from "vite";
import solid from "vite-plugin-solid";
import tailwind from 'tailwindcss'

export default defineConfig({
  plugins: [solid()],
  server: {
    historyApiFallback: true,
  },
  css: {
    postcss: "./postcss.config.js",
  },
});