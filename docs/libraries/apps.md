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

3. install

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
import Apps from "gi://AstalApps";

const apps = new Apps.Apps({
  includeEntry: true,
  includeExecutable: true,
});

print(apps.fuzzy_query("spotify")
    .map(app => app.name)
    .join("\n"))

```

```py [<i class="devicon-python-plain"></i> Python]
import gi

gi.require_version("AstalApps", "0.1")

from gi.repository import AstalApps

apps = AstalApps.Apps(include_entry = True, include_executable = True )

match = apps.fuzzy_query("obsidian")
print("\n".join(app.get_name() for app in match))
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local lgi = require("lgi")

local AstalApps = lgi.require("AstalApps", "0.1")

local apps = AstalApps.Apps({
	include_entry = true,
	include_executable = true,
})

local match = apps:fuzzy_query("lutris")

for _, app in ipairs(match) do
	print(app.name)
end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented, contributions are appreciated
```

:::

:::info
The fuzzy query uses [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance). I am not a mathematician, but if you know how to reimplement
the logic of [fzf](https://github.com/junegunn/fzf) to make it better feel free to open PRs.
:::
