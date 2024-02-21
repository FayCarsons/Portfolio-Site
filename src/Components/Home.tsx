import { onCleanup, onMount } from 'solid-js';
import { initShaders, shaderCleanup } from '../Sketches/clouds/main';
import { Github, Instagram, Twitter } from './Icons';

const Home = () => {
  onMount(() => {
    initShaders();
  });

  onCleanup(() => {
    shaderCleanup();
  });

  return (
    <div class="m-0 overflow-hidden p-0">
      <div id="app" class="flex h-[100dvh] w-[100dvw] flex-col">
        <div class="z-10 m-0 flex h-[100dvh] w-[100dvw] flex-col items-center justify-center overflow-hidden p-0">
          <h1
            id="name"
            class="aurora textstroke-3 mb-4 text-center text-7xl tracking-wider text-transparent"
          >
            Fay Carsons
          </h1>
          <div class="relative flex flex-row items-center justify-center space-x-2">
            <a
              aria-label="articles"
              class="roundpop tracking-wider text-white"
              href="/articles"
            >
              articles
            </a>
            <span class="mx-2 h-2 border-l border-white"></span>
            <a
              aria-label="sketches"
              class="roundpop tracking-wider text-white"
              href="/sketches"
            >
              sketches
            </a>
          </div>
        </div>
        <div
          id="links"
          class="z-10 mb-4 flex flex-row content-center justify-center"
        >
          <a
            class="mx-6 block w-4 stroke-white blur-0 transition duration-200 ease-in hover:blur-[1px]"
            href="https://instagram.com/faycarsons"
          >
            <Instagram class="h-8 w-8 stroke-white" />
          </a>
          <a
            class="mx-6 block w-4 stroke-white shadow-white blur-0 drop-shadow-sm transition duration-200 ease-in hover:blur-[1px]"
            href="https://twitter.com/fay_carsons"
          >
            <Twitter class="h-8 w-8 stroke-white" />
          </a>
          <a
            class="text-shadow mx-6 block w-4 stroke-white shadow-white blur-0 transition duration-200 ease-in hover:blur-[1px]"
            href="https://github.com/faycarsons"
          >
            <Github class="h-8 w-8 stroke-white" />
          </a>
        </div>
      </div>
      <canvas
        id="canvas"
        class="absolute inset-0 -z-10 m-0 h-[100dvh] w-[100dvw]"
      ></canvas>
    </div>
  );
};

export default Home;
