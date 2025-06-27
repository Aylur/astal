# Battery

Library and CLI tool for monitoring [upowerd](https://upower.freedesktop.org/)
devices.

## Usage

You can browse the
[Battery reference](https://aylur.github.io/libastal/battery).

### CLI

```sh
astal-battery --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Battery from "gi://AstalBattery"

const battery = Battery.get_default()

print(battery.percentage)
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalBattery as Battery

battery = Battery.get_default()

print(battery.get_percentage())
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Battery = require("lgi").require("AstalBattery")

local battery = Battery.get_default()

print(battery.percentage)
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::

## Installation

1. install dependencies

   :::code-group

   ```sh [<i class="devicon-archlinux-plain"></i> Arch]
   sudo pacman -Syu meson vala valadoc json-glib gobject-introspection
   ```

   ```sh [<i class="devicon-fedora-plain"></i> Fedora]
   sudo dnf install meson vala valadoc json-glib-devel gobject-introspection-devel
   ```

   ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
   sudo apt install meson valac valadoc libjson-glib-dev gobject-introspection
   ```

   :::

   ::: info

   Although UPower is not a direct build dependency, it should be
   self-explanatory that the daemon is required to be available at runtime.

   :::

2. clone repo

   ```sh
   git clone https://github.com/aylur/astal.git
   cd astal/lib/battery
   ```

3. install

   ```sh
   meson setup build
   meson install -C build
   ```
