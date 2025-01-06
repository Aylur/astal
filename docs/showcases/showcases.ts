type Showcase = {
    image: string
    url: string
    icon?: string // https://devicon.dev/
    title?: string
    description?: string
    author: string
}

// TODO: support more layouts
type Grid<T> = T | [T, T]

export default [
    {
        image: "/astal/showcase/aylur.webp",
        url: "https://github.com/Aylur/dotfiles",
        icon: "devicon-typescript-plain",
        title: "Marble Shell",
        author: "Aylur",
    },
    {
        image: "/astal/showcase/tokyob0t.webp",
        url: "https://github.com/tokyob0t/dotfiles",
        icon: "devicon-lua-plain",
        title: "Tokyob0t's Desktop",
        description: "Abandonad toda esperanza, vosotros que entráis aquí.",
        author: "tokyob0t",
    },
    {
        image: "/astal/showcase/kotontrion-kompass.webp",
        url: "https://github.com/kotontrion/kompass",
        icon: "devicon-vala-plain",
        title: "kompass",
        author: "kotontrion",
    },

    // add more showcases here~
] satisfies Array<Grid<Showcase>>
