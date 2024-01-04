import { posts, Post } from "./posts.ts";
import { Result, log } from "../lib/utilities.ts";
import { init_shaders } from "../sketches/clouds/clouds.ts";

const AUTHOR_NAME = "Fay Carsons";

let post_index = 0;
let post_cache: Post[] = [];

const p_class = "font-mono text-sm my-4 mx-auto";
const info_class = "font-mono text-gray-500 text-xs mb-2";
const h1_class = "text-3xl font-bold mb-2";
const err_div_class = "flex flex-col items-center justify-center";
const err_h2_class = "text-2xl font-bold"

const classes: { [key: string]: string } = {
  p: p_class,
  h1: h1_class,
};

// Helper for fetching & validating DOM elements - returns 'Result class'
const get_id = (id: string): Result<HTMLElement, Error> => {
  let maybe = document.getElementById(id);

  return !!maybe
    ? Result.ok(maybe)
    : Result.err(new Error(`Cannot access ${id} element`));
};

// Sets date and author of post
const set_post_info = (container: HTMLDivElement, date_string: string) => {
  const date = document.createElement("p");
  date.className = info_class;
  date.innerText = date_string;
  const name = document.createElement("p");
  name.className = info_class;
  name.innerText = AUTHOR_NAME;

  container.appendChild(date);
  container.appendChild(name);
};

// Gets blog root, creates element from Post object, appends to root, 
// and caches neighboring posts 
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

const set_error_message = () => {
  const maybe_post = get_id('post');
  const post = maybe_post.unwrap<HTMLDivElement>();
  const div = document.createElement('div');
  div.className = err_div_class;
  const err_msg = document.createElement('h2')
  err_msg.innerText = 'Oops we\'re having some problems :0'
  err_msg.className = err_h2_class
  div.appendChild(err_msg)
  post.appendChild(div)
}

// Fetches post from URL in posts[i]
const fetch_post = async (idx: number): Promise<Post> => {
  console.log('fetching !')
  const url = posts[idx];
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

// Caches the posts to the left and right of the current post 
// this is to decrease loading time and (hopefully) improve UX
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

// Creates onClick fn for nav buttons
const enum Direction {
  Left = -1,
  Right = 1,
}
const create_post_nav = (dir: Direction) => {
  return async (_: UIEvent) => {
    post_index = log((post_index + dir.valueOf()) % posts.length);
    if (post_index < 0) {
      post_index = posts.length - 1
    }
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

async function init(): Promise<Result<null, Error>> {
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
    return Result.err(e) as Result<null, Error>
  }

  init_shaders();
  return Result.ok(null)
}

 if (! (await init()).ok) {
    set_error_message()
 }
