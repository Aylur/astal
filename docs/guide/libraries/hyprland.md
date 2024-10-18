# Hyprland

Library and CLI tool for monitoring the [Hyprland socket](https://wiki.hyprland.org/IPC/).

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
cd astal/lib/hyprland
```

3. install

```sh
meson setup --prefix /usr build
meson install -C build
```

## Usage

You can browse the [Hyprland reference](https://aylur.github.io/libastal/hyprland).

### CLI

```sh
astal-hyprland # starts monitoring
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Hyprland from "gi://AstalHyprland"

const hyprland = Hyprland.get_default()

for (const client of hyprland.get_clients()) {
    print(client.title)
}
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalHyprland as Hyprland

hyprland = Hyprland.get_default()

for client in hyprland.get_clients():
    print(client.get_title())
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Hyprland = require("lgi").require("AstalHyprland")

local hyprland = Hyprland.get_default()

for _, c in ipairs(hyprland.clients) do
    print(c.title)
end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::
