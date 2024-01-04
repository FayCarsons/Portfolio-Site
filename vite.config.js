import { defineConfig } from "vite";
import { ViteMinifyPlugin } from "vite-plugin-minify";
import { resolve } from "path";

export default defineConfig({
  plugins: [ ViteMinifyPlugin({})],
  build: {
    minify: true,
    cssMinify: true,
    rollupOptions: {
      input: {
        index: resolve(__dirname, "index.html"),
        articles: resolve(__dirname, "articles.html"),
      },
    },
  },
});
