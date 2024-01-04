import { posts, Post } from "./posts.ts";
import { Result, log } from "../lib/utilities.ts";
import { init_shaders } from "../sketches/clouds/clouds.ts";

let post_index = 0;
let post_cache: Post[] = [];

const p_class = "font-mono text-sm my-4 mx-auto";
const info_class = "font-mono text-gray-500 text-xs mb-2";
const h1_class = "text-3xl font-bold mb-2";

const classes: { [key: string]: string } = {
  p: p_class,
  h1: h1_class,
};

const get_id = (id: string): Result<HTMLElement, Error> => {
  let maybe = document.getElementById(id);

  return !!maybe
    ? Result.ok(maybe)
    : Result.err(new Error(`Cannot access ${id} element`));
};

const set_post_info = (container: HTMLDivElement, date_string: string) => {
  const date = document.createElement("p");
  date.className = info_class;
  date.innerText = date_string;
  const name = document.createElement("p");
  name.className = info_class;
  name.innerText = "Fay Carsons";

  container.appendChild(date);
  container.appendChild(name);
};

const set_post = (post: Post): Result<null, Error> => {
  const maybe_header = get_id("header");
  if (!maybe_header.ok || !(maybe_header.inner instanceof HTMLElement)) {
    throw maybe_header;
  }
  const header_element = maybe_header.unwrap<HTMLDivElement>();
  const title = post.header.replace("<h1>", `<h1 class=\"${h1_class}\">`);
  header_element.innerHTML = title;

  document.title = title.replace(/<h1.*?>(.*?)<\/h1>/i, "$1");
  set_post_info(header_element, post.date);

  const maybe_post = get_id("post");
  if (!maybe_post.ok || !(maybe_post.inner instanceof HTMLElement)) {
    throw maybe_post;
  }
  const post_element = maybe_post.unwrap<HTMLDivElement>();
  const styled_content = post.content
    .map((elt) =>
      elt.replace(/<(\w+)([^>]*)>/, (_, tag) => {
        const elt_class = classes[tag] || "";
        return `<${tag} class=\"${elt_class}\">`;
      })
    )
    .join("");
  post_element.innerHTML = styled_content;
  cache_posts();  
  return Result.ok(null);
};

const enum Direction {
  Left = -1,
  Right = 1,
}

const fetch_post = async (i: number): Promise<Post> => {
  const url = posts[i];
  const req = await fetch(url);
  if (!req.ok) {
    throw new Error(req.statusText);
  }
  const json: Post = await req.json();
  if (!json) {
    console.error("JSON is ERROR :/");
    throw new Error(`Could not parse post ${url}`);
  }
  return json;
};

const cache_posts = async () => {
  for (
    let i = Math.abs(post_index - 1) % posts.length;
    i <= Math.abs(post_index + 1) % posts.length;
    i++
  ) {
    if ((!post_cache[i]) && posts[i]) {
      post_cache[i] = await fetch_post(i);
    }
  }

  console.log(post_cache);
};

const create_post_nav = (dir: Direction) => {
  return async (_: UIEvent) => {
    post_index = log(Math.abs(post_index + dir.valueOf()) % posts.length);
    try {
      const post = post_cache[post_index]
        ? post_cache[post_index]
        : await fetch_post(post_index);
      set_post(post);
    } catch (e) {
      console.error(e);
    }
  };
};

async function init() {
  const buttons: Result<HTMLElement, Error>[] = [
    get_id("left"),
    get_id("right"),
  ];
  if (
    buttons.some((res) => !res.ok || !(res.inner instanceof HTMLButtonElement))
  ) {
    console.error("Cannot find buttons!");
  }
  const [left, right] = buttons.map((r) => r.unwrap<HTMLButtonElement>());
  left.onclick = create_post_nav(Direction.Left);
  right.onclick = create_post_nav(Direction.Right);

  try {
    const post = await fetch_post(post_index);
    set_post(post);
  } catch (e) {
    console.error(e);
  }

  init_shaders();
}

init();
