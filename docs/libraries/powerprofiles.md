# Power Profiles

Library and CLI tool for monitoring [upowerd](https://upower.freedesktop.org/) powerprofiles.

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

::: info
Although UPower is not a direct build dependency,
it should be self-explanatory that the daemon is required to be available at runtime.
:::

2. clone repo

```sh
git clone https://github.com/aylur/astal.git
cd astal/lib/powerprofiles
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

You can browse the [PowerProfiles reference](https://aylur.github.io/libastal/powerprofiles).

### CLI

```sh
astal-power-profiles --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import PowerProfiles from "gi://AstalPowerProfiles"

const powerprofiles = PowerProfiles.get_default()

print(powerprofiles.activeProfile)
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalPowerProfiles as PowerProfiles

powerprofiles = PowerProfiles.get_default()

print(powerprofiles.get_active_profile())
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local PowerProfiles = require("lgi").require("AstalPowerProfiles")

local powerprofiles = PowerProfiles.get_default()

print(powerprofiles.active_profile)
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::
