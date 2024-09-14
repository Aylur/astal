# River

Library and CLI tool for monitoring the [River Wayland Compositor](https://isaacfreund.com/software/river/).

## Installation

1. install dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson json-glib gobject-introspection
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson gcc json-glib-devel gobject-introspection-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
sudo apt install meson libjson-glib-dev gobject-introspection
```

:::

2. clone repo

```sh
git clone https://github.com/aylur/astal.git
cd astal/lib/river
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

You can browse the [River reference](https://aylur.github.io/libastal/river).

### CLI

```sh
astal-river --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import River from "gi://AstalRiver";

const river = River.get_default();

console.log(river.get_outputs().map((o) => o.name));
```

```py [<i class="devicon-python-plain"></i> Python]
import gi

gi.require_version("AstalRiver", "0.1")

from gi.repository import AstalRiver

river = AstalRiver.get_default()

print("\n".join(o.get_name() for o in river.get_outputs()))
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local lgi = require("lgi")

local AstalRiver = lgi.require("AstalRiver", "0.1")

local river = AstalRiver.River.get_default()

for _, o in ipairs(river.outputs) do
	print(o.name)
end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::
