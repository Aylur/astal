# Theming

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
import style from "inline:./style.css"

const inlineCssInCode = `
window {
    background-color: transparent;
}
`

App.start({
    css: "./style.css",
    css: style,
    css: `${SRC}/style.css'`,
    css: inlineCss,
})
```

:::

:::info
When using AGS the global `SRC` will point to the directory `app.ts` is in.
AGS will set the current working directory to `--config`, so `./style.css` also works.
If you are not using AGS you should inline import CSS instead.
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

::: code-group

```sh [astal]
# to bring up the inspector run
astal --inspector
```

```sh [ags]
# to bring up the inspector run
ags --inspector
```

:::

## Using SCSS

Gtk's CSS only supports a subset of what the web offers.
Most notably nested selectors are unsupported by Gtk, but this can be
workaround by using preprocessors like [SCSS](https://sass-lang.com/).

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu dart-sass
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
npm install -g sass # not packaged on Fedora
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
npm install -g sass # not packaged on Ubuntu
```

:::

Importing `scss` files will simply return transpiled css.

:::code-group

```ts [app.ts]
import style from "./style.scss"

App.start({
    css: style,
    main() {},
})
```

:::

:::details Without AGS
AGS uses a plugin to transpile scss files before importing them.
If you are not using AGS, you can use a plugin for your chosen bundler.
:::

:::tip
If you for example want to set scss varibles from JS,
You can inline import, compose, and transpile yourself.

```ts [app.ts]
import style1 from "inline:./style1.scss"
import style2 from "inline:./style2.scss"

const tmpscss = "/tmp/style.scss"
const target = "/tmp/style.css"

writeFile(tmpscss, `
  $var1: red;
  $var1: blue;
  ${style1}
  ${style1}
`)

exec(`sass ${tmpscss} ${target}`)

App.start({
    css: target,
})

```

:::

:::info
If you want other preprocessors support builtin open an Issue.
:::
