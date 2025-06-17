// Types for your blog posts
interface BlogPost {
    title: string;
    date: string;
    slug: string;
    tags: string[];
    content: string;
}

// Function to load and render blog previews
export async function loadBlogPreviews() {
    const previewsContainer = document.getElementById('blog-previews');

    if (!previewsContainer) return;
    console.log('got blog container')

    // Show loading state
    previewsContainer.className = 'blog-previews loading';
    previewsContainer.textContent = 'Loading articles...';

    try {
        const response = await fetch('blogs.json');
        const posts: BlogPost[] = await response.json();

        // Clear loading state
        previewsContainer.className = 'blog-previews';
        previewsContainer.innerHTML = '';

        // Show latest 3-4 posts
        const recentPosts = posts.slice(0, 4) ?? [posts[0]];

        recentPosts.forEach(post => {
            const postElement = createBlogPreviewElement(post);
            previewsContainer.appendChild(postElement);
        });

    } catch (error) {
        console.error('Failed to load blog posts:', error);
        previewsContainer.className = 'blog-previews';
        previewsContainer.innerHTML = '<div style="color: #888; font-size: 0.85rem;">Failed to load articles</div>';
    }
}

// Create a blog preview element
function createBlogPreviewElement(post: BlogPost): HTMLElement {
    console.log("POST")
    console.table(post)
    const article = document.createElement('article');
    article.className = 'blog-preview-item';

    // Format date nicely
    const date = new Date(post.date);
    const formattedDate = date.toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'short'
    });

    // Limit tags to 2-3 for space
    const displayTags = post.tags

    article.innerHTML = `
    <h3>
      <a href="/blog/${post.slug}">${post.title}</a>
    </h3>
    <div class="blog-preview-meta">
      <span class="blog-preview-date">${formattedDate}</span>
      <div class="blog-preview-tags">
        ${displayTags.map(tag => `<a href=\'${tag}\'><span class="blog-preview-tag">${tag}</span></a>`).join('')}
      </div>
    </div>
    <p class="blog-preview-excerpt">${post.content}</p>
  `;

    return article;
}


// Optional: Refresh every 5 minutes if you update posts frequently
// setInterval(loadBlogPreviews, 5 * 60 * 1000);