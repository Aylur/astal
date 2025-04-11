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
    {
        image: "/astal/showcase/ezerinz.webp",
        url: "https://github.com/ezerinz/epik-shell",
        icon: "devicon-javascript-plain",
        title: "Epik Shell",
        author: "ezerinz",
    },
    {
        image: "/astal/showcase/hyprpanel_showcase.webp",
        url: "https://github.com/Jas-SinghFSU/hyprpanel",
        icon: "devicon-javascript-plain",
        title: "HyprPanel",
        author: "Jas",
    },
    {
        image: "/astal/showcase/unnamed-shell.webp",
        url: "https://codeberg.org/bunbun/unnamed-shell",
        icon: "devicon-vala-plain",
        title: "unnamed-shell",
        author: "bunbun",
    },

    // add more showcases here~
] satisfies Array<Grid<Showcase>>
