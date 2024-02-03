import { createEffect, createSignal } from "solid-js";
import { Post } from "../lib/posts";

export type BlogProps = {
  post: Post
};

const infoClass = "font-mono text-gray-500 text-xs mb-2";

export const Blog = ({post}: BlogProps) => {
  return (
    <section>
      <div id="header"innerHTML={post.header}>
        <p class={infoClass}>{post.date}</p>
        <p class={infoClass}>Fay Carsons</p>
      </div>
      <hr class="bg-gray-500 mx-auto w-[90%] my-4" />
      <div id="post" innerHTML={post.body.join()}/>
    </section>
  );
};
