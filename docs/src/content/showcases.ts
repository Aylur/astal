type Showcase = {
    author: string
    src: string
    url: string
}

type Grid<T> =
    | [T, T]
    | [T, T, T, T]

export const showcases: Array<Showcase | Grid<Showcase>> = [
    { author: "Aylur", src: "/astal/showcase/aylur1.png", url: "https://github.com/Aylur/dotfiles" },
]
