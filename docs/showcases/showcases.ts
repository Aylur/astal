type Showcase = {
    author: string
    src: string
    url: string
}

type Grid<T> = T
    | [T, T]
    | [T, T, T]
    | [T, T, T, T]

export default [
    { author: "Aylur", src: "/astal/showcase/aylur1.png", url: "https://github.com/Aylur/dotfiles" },
    // add more showcases here
] satisfies Array<Grid<Showcase>>
