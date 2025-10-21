# Astal Docs

This directory contains the Astal documentation and Library references. Hosted
at [aylur.github.io/astal](https://aylur.github.io/astal/) and
[aylur.github.io/libastal](https://aylur.github.io/libastal/)

## Commands

| Command            | Action                                      |
| :----------------- | :------------------------------------------ |
| `pnpm install`     | Installs dependencies                       |
| `pnpm run dev`     | Starts local dev server at `localhost:5173` |
| `pnpm run build`   | Build your production site to `./dist/`     |
| `pnpm run preview` | Preview your build locally                  |

## Add your creation to the showcases page

1. Add your image as a webp to `public/showcase`
2. Add it to `showcases/showcases.ts`
   - `src` should be `/astal/showcase/your-name-optional-title.webp`
   - `url` should point to the source code of the showcased widget/setup
   - `author` should be your name/nickname

```
.
├── public/showcase
│   └── your-name-optional-title.webp   # 1. add image
└── showcases/
    └── showcases.ts                    # 2. add information
```
