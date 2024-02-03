export const plugins = {
    tailwindcss: {},
    autoprefixer: {},
    cssnano: process.env.NODE_ENV === "production" ? {} : false,
};
