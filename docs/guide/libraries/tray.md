# Tray

Library for managing the systemtray by implementing the [StatusNotifierItem](https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/) protocol.

## Installation

1. install dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson gtk3 gobject-introspection libdbusmenu-gtk3
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson gcc gtk3-devel libdbusmenu-gtk3 gobject-introspection-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
sudo apt install meson libgtk-3-dev libdbusmenu-gtk3-dev gobject-introspection
```

:::

2. clone repo

```sh
git clone https://github.com/aylur/astal.git
cd astal/lib/tray
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

You can browse the [Tray reference](https://aylur.github.io/libastal/tray).

### CLI

```sh
astal-tray --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Tray from "gi://AstalTray"

const tray = Tray.get_default()

for (const item of tray.get_items()) {
    print(item.title)
}
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalTray as Tray

tray = Tray.get_default()

for item in tray.get_items():
    print(item.title)
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Tray = require("lgi").require("AstalTray")

local tray = Tray.get_default()

for _, i in ipairs(tray.items) do
    print(i.title)
end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::
