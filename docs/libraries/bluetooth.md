# Bluetooth

Library for monitoring [bluez](https://www.bluez.org/) over dbus.

## Installation

1. install dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson vala gobject-introspection
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson gcc valac gobject-introspection-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
sudo apt install meson valac gobject-introspection
```

:::

::: info
Although bluez is not a direct build dependency,
it should be self-explanatory that the daemon is required to be available at runtime.
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

:::tip
Most distros recommend manual installs in `/usr/local`,
which is what `meson` defaults to. If you want to install to `/usr`
instead which most package managers do, set the `prefix` option:

```sh
meson setup --prefix /usr build
```

:::

## Usage

You can browse the [Bluetooth reference](https://aylur.github.io/libastal/bluetooth).

### CLI

There is no CLI for this library, use the one provided by bluez.

```sh
bluetoothctl --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Bluetooth from "gi://AstalBluetooth";

const bluetooth = Bluetooth.get_default();

console.log(bluetooth.get_devices().map((d) => d.name));
```

```py [<i class="devicon-python-plain"></i> Python]
import gi

gi.require_version("AstalBluetooth", "0.1")

from gi.repository import AstalBluetooth

bluetooth = AstalBluetooth.get_default()

print("\n".join(d.get_name() for d in bluetooth.get_devices()))
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local lgi = require("lgi")

local AstalBluetooth = lgi.require("AstalBluetooth", "0.1")

local bluetooth = AstalBluetooth.get_default()

for _, d in ipairs(bluetooth.devices) do
	print(d.name)
end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::
