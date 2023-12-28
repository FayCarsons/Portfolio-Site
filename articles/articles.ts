import { posts, Post } from "./posts.ts";
import { Result } from "../lib/utilities.ts";
import {init_shaders} from '../sketches/clouds/clouds.ts'

let post_index = 0;

const p_class = "";
const h1_class = "";

const classes: { [key: string]: string } = {
  P: p_class,
  H1: h1_class,
};

const get_id = (id: string): Result<HTMLElement, Error> => {
  let maybe = document.getElementById(id);

  return !!maybe
    ? Result.ok(maybe)
    : Result.err(new Error(`Cannot access ${id} element`));
};

const apply_styles = (root: HTMLDivElement) => {
  const children = root.children;

  for (const child of children) {
    child.classList.add(classes[child.tagName] || "");
  }
};

const set_post = (post: Post): Result<HTMLDivElement, Error> => {
  const range = document.createRange();
  const maybe_container = get_id("blog");
  if (!maybe_container.ok) {
    return maybe_container as Result<HTMLDivElement, Error>;
  }
  const container = maybe_container.unwrap() as HTMLDivElement;
  container.innerHTML = "";
  const fragment = range.createContextualFragment(post.content);
  container.appendChild(fragment);
  return Result.ok(container);
};

const enum Direction {
  Left = -1,
  Right = 1,
}

const create_nav_onclick = (dir: Direction) => {
  return (_: UIEvent) => {
    post_index = (post_index + dir.valueOf()) % posts.length;
    try {
      const root = set_post(posts[post_index]);
      if (!root.ok || !(root.inner instanceof HTMLDivElement)) {
        console.error("cannot find root blog div");
      }
      apply_styles(root.unwrap() as HTMLDivElement);
    } catch (e) {
      console.error(e);
    }
  };
};

function init() {
  const buttons: Result<HTMLElement, Error>[] = [
    get_id("left_button"),
    get_id("right_button"),
  ];
  if (
    buttons.some((res) => !res.ok || !(res.inner instanceof HTMLButtonElement))
  ) {
    console.error("Cannot find buttons!");
  }
  const [left, right] = buttons.map((r) => r.unwrap() as HTMLButtonElement);
  left.onclick = create_nav_onclick(Direction.Left);
  right.onclick = create_nav_onclick(Direction.Right);

  const root = set_post(posts[0]);
  if (!root.ok || !(root.inner instanceof HTMLDivElement)) {
    console.error("cannot find root blog div");
  }

  apply_styles(root.unwrap() as HTMLDivElement);
  init_shaders();
}

init()

