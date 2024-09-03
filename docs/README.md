# Astal Docs

This directory contains the Astal documentation and Library references.
Hosted at [aylur.github.io/astal](https://aylur.github.io/astal/) and [aylur.github.io/libastal](https://aylur.github.io/libastal/)

## Commands

| Command           | Action                                      |
| :---------------- | :------------------------------------------ |
| `npm install`     | Installs dependencies                       |
| `npm run dev`     | Starts local dev server at `localhost:4321` |
| `npm run build`   | Build your production site to `./dist/`     |
| `npm run preview` | Preview your build locally                  |

## Add your creation to the landing page

1. Add your image as a webp to `public/showcase`
2. Add it to `src/content/showcases.ts`
    - `src` should be `/astal/showcase/your-name-optional-title.webp`
    - `url` should point to the source code of the showcased widget
    - `author` your name

```
.
├── public/showcase
│   └── your-name-optional-title.webp   # 1. add image
└── src/content/
    └── showcases.ts                    # 2. add information
```
