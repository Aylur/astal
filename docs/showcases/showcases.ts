type Showcase = {
    src: string
    url: string
    icon?: string // https://devicon.dev/
    title?: string
    description?: string
    author: string
}

type Grid<T> = T | [T, T] | [T, T, T] | [T, T, T, T]

export default [
    {
        src: "/astal/showcase/aylur.png",
        url: "https://github.com/Aylur/dotfiles",
        icon: "devicon-javascript-plain",
        description: "Placeholder (this is an ags v1 screenshot)",
        author: "Aylur",
    },
    {
        src: "/astal/showcase/tokyob0t-super-duper-hiper-mega-ultra-contribution.webp",
        url: "https://github.com/tokyob0t/dotfiles",
        icon: "devicon-lua-plain",
        title: "Tokyob0t's Desktop",
        description: "Abandonad toda esperanza, vosotros que entráis aquí.",
        author: "tokyob0t",
    },
    // add more showcases here~
] satisfies Array<Grid<Showcase>>
