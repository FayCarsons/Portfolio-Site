import { Ref, onCleanup, onMount } from 'solid-js';
import { initShaders, shaderCleanup } from '../Sketches/clouds/main';
import { Home } from './Icons';

const Sketches = () => {
  onMount(() => initShaders());
  onCleanup(() => {
    shaderCleanup();
  });

  return (
    <section class="inset-0 m-0 h-[100dvh] w-[100dvw] p-0">
      <a href="/">
        <Home class="absolute left-2 top-2 stroke-white shadow-white blur-0 transition duration-200 ease-in hover:blur-[1px]" />
      </a>
      <div class="z-10 flex h-[100dvh] w-[100dvw] flex-col items-center justify-center">
        <h1
          id="name"
          class="aurora textstroke-3 mb-4 text-center text-7xl tracking-wider text-transparent"
        >
          Coming Soon
        </h1>
      </div>
      <canvas
        id="canvas"
        class="absolute inset-0 -z-10 m-0 h-screen w-screen p-0"
      ></canvas>
    </section>
  );
};

export default Sketches;
