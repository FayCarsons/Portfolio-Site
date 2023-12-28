import * as fs from "fs";
import * as path from "path";
import * as marked from "marked"; // You may need to install this package using: npm install marked
import * as prettier from "prettier";

const markdownFolder = "./markdown";
const outputFilePath = "./articles/posts.ts";
const post_type =
  "export type Post = {name: string, header: string, content: string}";

// Read the Markdown files from the folder
const markdownFiles = fs.readdirSync(markdownFolder);

// Convert each Markdown file to TSX
const tsxFiles = await Promise.all(
  markdownFiles.map((markdownFile) => {
    const markdownContent = fs.readFileSync(
      path.join(markdownFolder, markdownFile),
      "utf-8"
    );
    const name = markdownFile
      .split(".")[0]
      .toLowerCase()
      .split("")
      .filter((char) => /[a-z]/i.test(char))
      .join("");
    const html = marked.parse(markdownContent);
    const header_regex = /<h1>(.*?)<\/h1>/;
    const header_match = html.match(header_regex);
    const header = header_match ? header_match[0] : null;
    if (header) {
      console.log("Got header")
      const content = html.replace(header_regex, '');
      return `{name: \"${name}\", header: \"${header}\", content: \`${content}\`}`
    } else {
      console.error("did not get header")
    }
    ;
  })
);

// Generate the TypeScript file with an array containing all TSX files
const content = `${post_type}\n\n export const posts: Post[] = [\n${tsxFiles.join(
  ""
)}\n];`;

const formatted_content = await prettier.format(content, {
  parser: "typescript",
});

// Write the content to the output file
fs.writeFileSync(outputFilePath, formatted_content, "utf-8");

console.log("Markdown conversion complete");
