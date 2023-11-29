import { defineConfig } from "vite";
import { viteSingleFile } from "vite-plugin-singlefile";
import {ViteMinifyPlugin} from 'vite-plugin-minify'
import glsl from 'vite-plugin-glsl';

export default defineConfig({
    plugins: [glsl(), viteSingleFile({ removeViteModuleLoader: true}), ViteMinifyPlugin({})],
    build: {
        minify: true,
        cssMinify: true,
    }
})