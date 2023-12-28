import { defineConfig } from "vite";
import { ViteMinifyPlugin } from "vite-plugin-minify";
import { resolve } from "path";
import {execSync} from 'child_process'

const md_convert = () => {
  return {
    name: "md-convert",
    buildstart() {
      execSync('node md_convert.js')
    }
  }
}

export default defineConfig({
  plugins: [md_convert(), ViteMinifyPlugin({})],
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
