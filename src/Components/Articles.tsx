// OLD VANILLA IMPL

import {
  Match,
  Setter,
  Switch,
  createEffect,
  createResource,
  createSignal,
  onMount,
} from 'solid-js';
import { posts, Post } from '../lib/posts';
import { Blog } from './Blog';
import { initShaders } from '../../src/Sketches/clouds/main';
import { range } from '../../src/lib/utilities';
import { useParams } from '@solidjs/router';

let postCache: Post[] = [];

export const titleToUrl = (title: string): string => {
  return `/json/${title}.json`;
};

// Fetches post from URL in posts[i]
const fetchPost = async (idx: number): Promise<Post> => {
  if (postCache[idx]) {
    return postCache[idx];
  } else {
    const url = titleToUrl(posts[idx]);
    const req = await fetch(url);
    if (!req.ok) {
      throw new Error(
        `Failed to fetch post ${url}, Status: ${req.status} - ${req.statusText}`,
      );
    }
    const json: Post = await req.json();
    if (!json) {
      throw new Error(`Could not parse post ${url}`);
    }
    postCache[idx] = json;
    return json;
  }
};

const boundIdx = (val: number): number => {
  return ((val % posts.length) + posts.length) % posts.length;
};

// Caches the posts to the left and right of the current post
// this is to decrease loading time and (hopefully) improve UX
const cachePosts = async (idx: number) => {
  let [start, end] = [boundIdx(idx - 1), boundIdx(idx + 1)];
  await Promise.all(
    range(start, end)
      .filter((i) => !postCache[i] && posts[i])
      .map(async (i) => (postCache[i] = await fetchPost(i))),
  );
};

// Creates onClick fn for nav buttons
const enum Direction {
  Left = -1,
  Right = 1,
}

type MaybePost = Post | null | Error;

const createPostNav = (setIndex: Setter<number>, dir: Direction) => {
  return async (_: UIEvent) => {
    setIndex((idx) => boundIdx(idx + dir));
  };
};

const Articles = () => {
  const { title } = useParams();
  const [idx, setIdx] = createSignal<number>(0);

  if (title) {
    setIdx(posts.indexOf(title));
  }

  const [post] = createResource(idx(), fetchPost);

  createEffect(() => {
    cachePosts(idx());
  }, idx());

  onMount(() => {
    initShaders();
  });

  return (
    <div class="m-0 flex h-screen w-screen items-center justify-center overflow-hidden p-0">
      <section
        aria-label="blog container"
        id="blog"
        class="z-10 h-5/6 w-3/4 overflow-y-scroll rounded-sm bg-gray-50 p-4 shadow-lg"
      >
        <Switch>
          <Match when={!post()}>
            <div class="animate-bounce"></div>
          </Match>
          <Match when={post() instanceof Error}>
            <Err />
          </Match>
          <Match when={post()}>
            <Blog post={post() as Post}></Blog>
          </Match>
        </Switch>
      </section>

      <button
        onClick={createPostNav(setIdx, Direction.Left)}
        class="absolute left-2 top-1/2 z-10 -translate-y-1/2 transform p-2"
      >
        <svg
          xmlns="http://www.w3.org/2000/svg"
          fill="none"
          viewBox="0 0 24 24"
          stroke="currentColor"
          class="h-6 w-6 text-white"
        >
          <path
            stroke-linecap="round"
            stroke-linejoin="round"
            stroke-width="2"
            d="M15 19l-7-7 7-7"
          />
        </svg>
      </button>

      <button
        onClick={createPostNav(setIdx, Direction.Right)}
        class="absolute right-2 top-1/2 z-10 -translate-y-1/2 transform p-2"
      >
        <svg
          xmlns="http://www.w3.org/2000/svg"
          fill="none"
          viewBox="0 0 24 24"
          stroke="currentColor"
          class="h-6 w-6 text-white"
        >
          <path
            stroke-linecap="round"
            stroke-linejoin="round"
            stroke-width="2"
            d="M9 5l7 7-7 7"
          />
        </svg>
      </button>
      <canvas
        id="canvas"
        class="absolute -z-10 m-0 h-[100dvh] w-[100dvw] p-0"
      ></canvas>
    </div>
  );
};

const Err = () => {
  return (
    <div class="flex flex-col items-center justify-center">
      <h2 class="text-2xl font-bold">Oops we're having some issues :0</h2>
    </div>
  );
};

export default Articles;
