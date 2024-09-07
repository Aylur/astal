# Apps

Library and CLI tool for querying and launching
applications that have a corresponding `.desktop` file.

## Installation

1. install dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson vala json-glib gobject-introspection
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson gcc valac json-glib-devel gobject-introspection-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
sudo apt install meson valac libjson-glib-dev gobject-introspection
```

:::

2. clone repo

```sh
git clone https://github.com/aylur/astal.git
cd astal/lib/apps
```

3. clone repo

```sh
meson setup build
meson install -C build
```

:::tip
Most distros recommend manual installs in `/usr/local`,
which is what `meson` defaults to. If you want to install to `/usr`
instead which most package managers do, set the `prefix` option:

```sh
meson setup --prefix /usr build
meson install -C build
```

:::

## Usage

You can browse the [Apps reference](https://aylur.github.io/libastal/apps).

### CLI

```sh
astal-apps --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Apps from "gi://AstalApps"

const apps = new Apps.Apps({
    includeEntry: true,
    includeExecutable: true,
})

print(apps.fuzzy_query("spotify")
    .map(app => app.name)
    .join("\n"))
```

```py [<i class="devicon-python-plain"></i> Python]
# Not yet documented, contributions are appreciated
```

```lua [<i class="devicon-lua-plain"></i> Lua]
-- Not yet documented, contributions are appreciated
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented, contributions are appreciated
```

:::

:::info
The fuzzy query uses [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance). I am not a mathematician, but if you know how to reimplement
the logic of [fzf](https://github.com/junegunn/fzf) to make it better feel free to open PRs.
:::
