# Bluetooth

Library for monitoring [bluez](https://www.bluez.org/) over dbus.

## Usage

You can browse the
[Bluetooth reference](https://aylur.github.io/libastal/bluetooth).

### CLI

There is no CLI for this library, use the one provided by bluez.

```sh
bluetoothctl --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Bluetooth from "gi://AstalBluetooth"

const bluetooth = Bluetooth.get_default()

for (const device of bluetooth.get_devices()) {
  print(device.name)
}
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalBluetooth as Bluetooth

bluetooth = Bluetooth.get_default()

for device in bluetooth.get_devices():
    print(device.get_name())
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Bluetooth = require("lgi").require("AstalBluetooth")

local bluetooth = Bluetooth.get_default()

for _, d in ipairs(bluetooth.devices) do
    print(d.name)
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
   sudo pacman -Syu meson vala valadoc gobject-introspection
   ```

   ```sh [<i class="devicon-fedora-plain"></i> Fedora]
   sudo dnf install meson vala valadoc gobject-introspection-devel
   ```

   ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
   sudo apt install meson valac valadoc gobject-introspection
   ```

   :::

   ::: info

   Although bluez is not a direct build dependency, it should be
   self-explanatory that the daemon is required to be available at runtime.

   :::

2. clone repo

   ```sh
   git clone https://github.com/aylur/astal.git
   cd astal/lib/bluetooth
   ```

3. install

   ```sh
   meson setup build
   meson install -C build
   ```
