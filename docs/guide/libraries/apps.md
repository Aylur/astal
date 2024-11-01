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
sudo dnf install meson vala valadoc json-glib-devel gobject-introspection-devel
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
meson setup --prefix /usr build
meson install -C build
```

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

for (const app of apps.fuzzy_query("spotify")) {
    print(app.name)
}
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalApps as Apps

apps = Apps.Apps(
    include_entry=True,
    include_executable=True,
)

for app in apps.fuzzy_query("obsidian"):
    print(app.get_name())

```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Apps = require("lgi").require("AstalApps")

local apps = Apps.Apps({
    include_entry = true,
    include_executable = true,
})

for _, app in ipairs(apps:fuzzy_query("lutris")) do
    print(app.name)
end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented, contributions are appreciated
```

:::
