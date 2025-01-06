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

You can pass a path to a file or css as a string in `App:start`

:::code-group

```lua [init.lua]
local inline_css = [[
    window {
        background-color: transparent;
    }
]]

App:start({
    css = inline_css,
    css = "/path/to/style.css",
    css = "./style.css",
})
```

:::

:::warning
When using relative paths, for example `./style.css` keep in mind that they
will be relative to the current working directory.
:::

## Css Property on Widgets

```lua
Widget.Label({
    css = "color: blue; padding: 1em;",
    label = "hello"
})
```

:::info
The `css` property of a widget will not cascade to its children.
:::

## Apply Stylesheets at Runtime

You can apply additional styles at runtime.

```lua
App:apply_css("/path/to/file.css")
```

```lua
App:apply_css([[
    window {
        background-color: transparent;
    }
]])
```

```lua
App:reset_css() -- reset if need
```

:::warning
`App:apply_css` will apply on top of other stylesheets applied before.
You can reset stylesheets with `App:reset_css`
or by passing `true` as a second parameter to `App:apply_css`.
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

```lua [init.lua]
local scss = "./style.scss"
local css = "/tmp/style.css"

astal.exec(string.format("sass %s %s", scss, css))

App:start({
    css = css,
})
```

:::
