// OLD VANILLA IMPL

import {
  Match,
  Setter,
  Switch,
  createEffect,
  createResource,
  createSignal,
  onCleanup,
  onMount,
} from 'solid-js';
import { posts, Post } from '../lib/posts';
import { Blog } from './Blog';
import { initShaders, shaderCleanup } from '../../src/Sketches/clouds/main';
import { dec, inc, range } from '../../src/lib/utilities';
import { useNavigate, useParams } from '@solidjs/router';

import { Home, LeftChevron, RightChevron } from './Icons';

let postCache: Post[] = [];

export const titleToUrl = (title: string): string => {
  return `/json/${title}.json`;
};

// Fetches post from URL in posts[i]
const fetchPost = async (idx: number, opt?: any): Promise<Post> => {
  if (postCache[idx]) {
    return Promise.resolve(postCache[idx]);
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
enum Direction {
  Left,
  Right,
}

const dirToFn = {
  [Direction.Left]: dec,
  [Direction.Right]: inc,
};

const createPostNav = (setIndex: Setter<number>, dir: Direction) => {
  return async (_: UIEvent) => setIndex((idx) => boundIdx(dirToFn[dir](idx)));
};
const chevronClass = 'h-6 w-6 text-white';

const Articles = () => {
  onMount(() => {
    initShaders();
  });

  onCleanup(() => {
    shaderCleanup();
  });

  const { title } = useParams();
  const navigator = useNavigate();
  const [idx, setIdx] = createSignal<number>(0);

  createEffect(() => {
    if (title) {
      setIdx(posts.indexOf(title));
    }
  }, title);

  const [post] = createResource(idx, fetchPost);

  createEffect(() => {
    cachePosts(idx());
    if (window.location.pathname === '/articles')
      navigator(`${window.location.pathname}#${posts[idx()]}`, {
        replace: true,
      });
  }, idx);

  return (
    <>
      <div class="m-0 flex h-[100dvh] w-[100dvw] items-center justify-center overflow-hidden p-0">
        <a href="/">
          <Home class="absolute left-2 top-2 z-10 stroke-white shadow-white blur-0 transition duration-200 ease-in hover:blur-[1px]" />
        </a>
        <section
          aria-label="blog post"
          id="blog"
          class="z-10 h-5/6 w-3/4 overflow-y-scroll rounded-sm bg-gray-50 p-4 shadow-lg"
        >
          <Switch>
            <Match when={post.loading}>
              <div class="animate-pulse"></div>
            </Match>
            <Match when={post.error}>
              <Err />
            </Match>
            <Match when={post.latest}>
              <Blog post={post.latest as Post}></Blog>
            </Match>
          </Switch>
        </section>

        <button
          aria-label="Last blog post"
          onClick={createPostNav(setIdx, Direction.Left)}
          class="absolute left-2 top-1/2 z-10 -translate-y-1/2 transform p-2"
        >
          <LeftChevron class={chevronClass} />
        </button>

        <button
          aria-label="Next blog post"
          onClick={createPostNav(setIdx, Direction.Right)}
          class="absolute right-2 top-1/2 z-10 -translate-y-1/2 transform p-2"
        >
          <RightChevron class={chevronClass} />
        </button>
      </div>
      <canvas
        id="canvas"
        class="absolute inset-0 -z-10 m-0 h-[100dvh] w-[100dvw]"
      />
    </>
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
