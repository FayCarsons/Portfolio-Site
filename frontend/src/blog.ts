
type Post = {
    date: string,
    title: string,
    slug: string,
    tags: string[],
    content: string
}

function compareDates(a: Post, b: Post): number {
    return new Date(b.date).getTime() - new Date(a.date).getTime()
}

export async function loadBlogPreviews() {
    const previewsContainer = document.getElementById('blog-previews');

    if (!previewsContainer) return;
    console.log('got blog container')

    // Show loading state
    previewsContainer.className = 'blog-previews loading';
    previewsContainer.textContent = 'Loading articles...';

    try {
        const response = await fetch('blogs.json');
        let posts = await response.json() as Post[];
        posts.sort(compareDates)

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
function createBlogPreviewElement(post: Post) {
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
    <div class="blog-preview">
        <a href="articles/${post.slug}" class="blog-preview-link">
            <h3>${post.title}</h3>
            <span class="blog-preview-date">${formattedDate}</span>
            <p class="blog-preview-excerpt">${post.content}</p>
        </a>
        <div class="blog-preview-tags">
            ${displayTags.map(tag => `<a href="tags/${tag}" class="blog-preview-link"><span class="blog-preview-tag">${tag}</span></a>`).join('')}
        </div>
    </div>
  `;

    return article;
}

loadBlogPreviews()