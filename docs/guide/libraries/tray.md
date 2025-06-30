# Tray

Library for managing the systemtray by implementing the
[StatusNotifierItem](https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/)
protocol.

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

## Installation

1. install dependencies

   :::code-group

   ```sh [<i class="devicon-archlinux-plain"></i> Arch]
   sudo pacman -Syu meson json-glib gobject-introspection
   ```

   ```sh [<i class="devicon-fedora-plain"></i> Fedora]
   sudo dnf install meson json-glib-devel gobject-introspection-devel
   ```

   ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
   sudo apt install meson libjson-glib-dev gobject-introspection
   ```

   :::

2. install `appmenu-glib-translator`

   ```sh
   git clone https://github.com/rilian-la-te/vala-panel-appmenu.git
   cd vala-panel-appmenu/subprojects/appmenu-glib-translator
   meson setup build
   meson install -C build
   ```

3. clone repo

   ```sh
   git clone https://github.com/aylur/astal.git
   cd astal/lib/tray
   ```

4. install

   ```sh
   meson setup build
   meson install -C build
   ```
