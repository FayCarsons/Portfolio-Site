import * as fs from "fs";
import * as path from "path";
import * as marked from "marked"; // You may need to install this package using: npm install marked
import * as prettier from "prettier";

const markdownFolder = "./markdown";
const js_output_path = "./articles/posts.ts";
const json_path = "./resources/json/"

const post_type =
  "export type Post = {name: string, date: string, header: string, content: string[]}";

// Read the Markdown files from the folder
const markdownFiles = fs.readdirSync(markdownFolder);

function split_date(html) {
  const date_regex = /Date: (\w+ \d{1,2}, \d{4})/;
  const match = html.match(date_regex);
  if (!match) {
    throw new Error('blog must contain date written in format \'Month day, year\'')
  }
  return [html.replace(date_regex, ''), match ? match[1] : null]
}

function sort_by_date(a, b) {
  const date_a = new Date(a.date);
  const date_b = new Date(b.date);

  if (date_a > date_b) return -1;
  if (date_a < date_b) return 1;
  return 0
}

// Convert each Markdown file to TSX
let posts = await Promise.all(
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
    const [html_wo_date, date] = split_date(html);
    const header_regex = /<h1>(.*?)<\/h1>/;
    const header_match = html_wo_date.match(header_regex);
    const header = header_match ? header_match[0] : null;
    if (header) {
      const content = html_wo_date.replace(header_regex, '').replace('\n', '').match(/<p>(?!\s*<\/p>)(.*?)<\/p>/g);
      const indented = content.map(s => s.replace(/<p>(.*?)<\/p>/, (match, text) => {
        const indented_text = `\t${text}`;
        return `<p>${indented_text}</p>`
      }));
      const json = {name, date, header, content: indented}
      return json
    } else {
      throw new Error(`Blog post ${name} does not contain header!`);
    }
    ;
  })
);

posts.sort(sort_by_date);
posts.forEach(async post => {
  const json = JSON.stringify(post);
  const formatted = await prettier.format(json, {
    parser: 'json'
  })
  fs.writeFileSync(json_path + post.name + '.json', formatted)
})
const json_paths = posts.map((post => '\'' + json_path + post.name + '.json\''))

// Generate the TypeScript file with an array containing all TSX files
const content = `${post_type}\n\n export const posts: string[] = [\n${json_paths.join(
  ", "
)}\n];`;

const formatted_content = await prettier.format(content, {
  parser: "typescript",
});

// Write the content to the output file
fs.writeFileSync(js_output_path, formatted_content, "utf-8");

console.log("Markdown conversion complete");
