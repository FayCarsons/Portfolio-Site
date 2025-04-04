import { posts } from 'posts.js'

const state = { index: 0, cache: [] }

const Dir = {
  Left: -1,
  Right: 1
}

async function fetchBlog(dir) {
  const idx = Math.abs(state.index + dir) % posts.length
  const res = state.cache[idx] ?? await fetch(posts[idx])
  if (res.ok) {
    const text = await res.text()
    const root = document.getElementById("article-container")
    root.innerHTML = text
    state.cache[idx] = text
    state.index = idx
  }
}
