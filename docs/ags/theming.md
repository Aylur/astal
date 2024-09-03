---
title: Theming
description: GTK3 CSS theming
sidebar:
    order: 1
---

Since the widget toolkit is **GTK3** theming is done with **CSS**.

- [CSS tutorial](https://www.w3schools.com/css/)
- [GTK CSS Overview wiki](https://docs.gtk.org/gtk3/css-overview.html)
- [GTK CSS Properties Overview wiki](https://docs.gtk.org/gtk3/css-properties.html)

:::warning GTK is not the web
While most features are implemented in GTK,
you can't assume anything that works on the web will work with GTK.
Refer to the [GTK docs](https://docs.gtk.org/gtk3/css-overview.html)
to see what is available.
:::

So far every widget you made used your default GTK3 theme.
To make them more custom, you can apply stylesheets to them.

## From file at startup

You can pass a path to a file or css as a string in `App.start`

:::code-group

```ts [app.ts]
const inlineCss = `
window {
    background-color: transparent;
}
`

App.start({
    css: "/home/username/.config/ags/style.css",
    css: `${SRC}/style.css'`,
    css: inlineCss,
})
```

:::

:::info
The global `SRC` will point to the directory `app.ts` is in.
You can pass a relative path, but its resolution will be relative to the current working directory.
:::

## Css Property on Widgets

```ts
Widget.Label({
    css: "color: blue; padding: 1em;",
    label: "hello",
})
```

:::info
The `css` property of a widget will not cascade to its children.
:::

## Apply Stylesheets at Runtime

You can apply additional styles at runtime.

```ts
App.apply_css("/path/to/file.css")
```

```ts
App.apply_css(`
window {
    background-color: transparent;
}
`)
```

```ts
App.reset_css() // reset if need
```

:::warning
`App.apply_css` will apply on top of other stylesheets applied before.
You can reset stylesheets with `App.resetCss`
:::

## Inspector

If you are not sure about the widget hierarchy or any CSS selector,
you can use the [GTK inspector](https://wiki.gnome.org/Projects/GTK/Inspector)

```sh
# to bring up the inspector run
ags --inspector
```

## Using pre-processors like SCSS

:::code-group

```ts [app.ts]
// main scss file
const scss = `${SRC}/style.scss`

// target css file
const css = `/tmp/my-style.css`

// make sure sassc is installed on your system
exec(`sassc ${scss} ${css}`)

App.config({
    css,
    main() {},
})
```

:::
