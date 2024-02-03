import { onMount } from "solid-js";
import { initShaders } from "../Sketches/clouds/main";

const Home = () => {
  onMount(initShaders);

  return (
    <div class="overflow-hidden m-0 p-0">
      <div id="app" class="w-screen h-screen flex flex-col">
        <div class="flex flex-col justify-center items-center w-screen h-screen z-10 m-0 p-0 overflow-hidden">
          <h1
            id="name"
            class="aurora textstroke-3 text-7xl text-transparent mb-4 text-center tracking-wider"
          >
            Fay Carsons
          </h1>
          <div class="relative flex flex-row justify-center items-center space-x-2">
            <a
              aria-label="articles"
              class="roundpop text-white tracking-wider"
              href="/articles"
            >
              articles
            </a>
            <span class="h-2 border-l mx-2 border-white"></span>
            <a
              aria-label="sketches"
              class="roundpop text-white tracking-wider"
              href="/sketches"
            >
              sketches
            </a>
          </div>
        </div>
        <div
          id="links"
          class="mb-4 flex flex-row justify-center content-center z-10"
        >
          <a
            class="block w-4 mx-6 stroke-white blur-0 transition duration-200 ease-in hover:blur-[1px]"
            href="https://instagram.com/faycarsons"
          >
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="32"
              height="32"
              viewBox="0 0 24 24"
              fill="none"
              stroke="white"
              stroke-width="2"
              stroke-linecap="round"
              stroke-linejoin="round"
              class="feather"
            >
              <rect x="2" y="2" width="20" height="20" rx="5" ry="5"></rect>
              <path d="M16 11.37A4 4 0 1 1 12.63 8 4 4 0 0 1 16 11.37z"></path>
              <line x1="17.5" y1="6.5" x2="17.51" y2="6.5"></line>
            </svg>
          </a>
          <a
            class="block w-4 mx-6 stroke-white drop-shadow-sm shadow-white blur-0 transition duration-200 ease-in hover:blur-[1px]"
            href="https://twitter.com/fay_carsons"
          >
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="32"
              height="32"
              viewBox="0 0 24 24"
              fill="none"
              stroke="white"
              stroke-width="2"
              stroke-linecap="round"
              stroke-linejoin="round"
              class="feather"
            >
              <path d="M23 3a10.9 10.9 0 0 1-3.14 1.53 4.48 4.48 0 0 0-7.86 3v1A10.66 10.66 0 0 1 3 4s-4 9 5 13a11.64 11.64 0 0 1-7 2c9 5 20 0 20-11.5a4.5 4.5 0 0 0-.08-.83A7.72 7.72 0 0 0 23 3z"></path>
            </svg>
          </a>
          <a
            class="block w-4 mx-6 stroke-white text-shadow shadow-white blur-0 transition duration-200 ease-in hover:blur-[1px]"
            href="https://github.com/faycarsons"
          >
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="32"
              height="32"
              viewBox="0 0 24 24"
              fill="none"
              stroke="white"
              stroke-width="2"
              stroke-linecap="round"
              stroke-linejoin="round"
              class="feather"
            >
              <path d="M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22"></path>
            </svg>
          </a>
        </div>
      </div>
      <canvas
        id="canvas"
        class="absolute inset-0 h-[100dvh] w-[100dvw] m-0 p-0 z-0"
      ></canvas>
    </div>
  );
};

export default Home