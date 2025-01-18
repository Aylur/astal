# Theming

Since the widget toolkit is **GTK** theming is done with **CSS**.

- [CSS tutorial](https://www.w3schools.com/css/)
- [GTK3 CSS Overview wiki](https://docs.gtk.org/gtk3/css-overview.html)
- [GTK3 CSS Properties Overview wiki](https://docs.gtk.org/gtk3/css-properties.html)
- [GTK4 CSS Overview wiki](https://docs.gtk.org/gtk4/css-overview.html)
- [GTK4 CSS Properties Overview wiki](https://docs.gtk.org/gtk4/css-properties.html)

:::warning GTK is not the web
While most features are implemented in GTK,
you can't assume anything that works on the web will work with GTK.
Refer to the GTK docs to see what is available.
:::

So far every widget you made used your default GTK theme.
To make them more custom, you can apply stylesheets to them.

## From file at startup

You can pass a path to a file or CSS as a string in `App.start`

:::code-group

```ts [app.ts]
const inlineCss = `
    window {
        background-color: transparent;
    }
`

App.start({
    css: inlineCss,
    css: "./style.css",
    css: "/path/to/style.css",
})
```

:::

:::warning
When using relative paths, for example `./style.css` keep in mind that they
will be relative to the current working directory.
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
astal --inspector
```

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

:::code-group

```ts [app.ts]
import { exec } from "astal/process"

exec("sass ./style.scss /tmp/style.css")

App.start({
    css: "/tmp/style.css",
    main() {},
})
```

:::

:::tip
You could also transpile scss into css using a bundler
and simply passing the path of the resulting css file to `css`.
:::
