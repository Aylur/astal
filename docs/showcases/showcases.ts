type Showcase = {
    label: string
    src: string
    url: string
}

type Grid<T> = T
    | [T, T]
    | [T, T, T]
    | [T, T, T, T]

export default [
    { label: "Placeholder (this is an ags v1 screenshot)", src: "/astal/showcase/aylur1.png", url: "https://github.com/Aylur/dotfiles" },
    // add more showcases here
] satisfies Array<Grid<Showcase>>
