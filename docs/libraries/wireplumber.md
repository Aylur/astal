# Wire Plumber

Wrapper library over [wireplumber](https://pipewire.pages.freedesktop.org/wireplumber/) to better integrate with Astal.

## Installation

1. install dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson vala wireplumber gobject-introspection
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson gcc valac wireplumber-devel gobject-introspection-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
# Not yet documented
```

:::

2. clone repo

```sh
git clone https://github.com/aylur/astal.git
cd astal/lib/wireplumber
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

You can browse the [Wireplumber reference](https://aylur.github.io/libastal/wireplumber).

### CLI

There is no CLI for this library, use the one provided by wireplumber.

```sh
wpctl --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Wp from "gi://AstalWp"

const audio = Wp.get_default_wp().audio

print(audio.default_speaker.volume)
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalWp as Wp

audio = Wp.get_default_wp().get_audio()

print(audio.get_default_speaker().get_volume())
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Wp = require("lgi").require("AstalWp")

local audio = Wp.get_default_wp().audio

print(audio.default_speaker.volume)
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::
