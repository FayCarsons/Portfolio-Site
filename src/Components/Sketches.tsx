import { Ref, onCleanup, onMount } from 'solid-js';
import { initShaders } from '../Sketches/clouds/main';
import { Home } from './Icons';

const Sketches = () => {
  onMount(() => initShaders());

  return (
    <div class="inset-0 m-0 h-screen w-screen p-0">
      <a href="/">
        <Home class="absolute left-2 top-2 stroke-white shadow-white blur-0 transition duration-200 ease-in hover:blur-[1px]" />
      </a>
      <div class="z-10 flex h-screen w-screen flex-col items-center justify-center">
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
    </div>
  );
};

export default Sketches;
