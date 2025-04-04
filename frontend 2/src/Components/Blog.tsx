import { Post } from "../lib/posts";

export type BlogProps = {
  post: Post;
};

const infoClass = "font-mono text-gray-500 text-xs mb-2";

export const Blog = ({ post }) => {
  return (
    <section aria-roledescription="Blog post">
      <div aria-roledescription="header" innerHTML={post.header} />
      <p class={infoClass}>{post.date}</p>
      <p class={infoClass}>Fay Carsons</p>
      <hr class="mx-auto my-4 w-[90%] bg-gray-500" />
      <div id="post" innerHTML={post.body} />
    </section>
  );
};
