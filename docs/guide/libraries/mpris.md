# Mpris

Library and CLI tool for interacting and monitoring media players exposing an
mpris interface through dbus.

An alternative for [playerctl](https://github.com/altdesktop/playerctl) that
better integrates with astal.

## Usage

You can browse the [Mpris reference](https://aylur.github.io/libastal/mpris).

### CLI

```sh
astal-mpris --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Mpris from "gi://AstalMpris"

const spotify = Mpris.Player.new("spotify")

if (spotify.available) print(spotify.title)
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalMpris as Mpris

spotify = Mpris.Player.new("spotify")

if spotify.get_available():
    print(spotify.get_title())
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Mpris = require("lgi").require("AstalMpris")

local spotify = Mpris.Player.new("spotify")

if spotify.available then
    print(spotify.title)
end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::

## Installation

> [!WARNING]
>
> In order for network cover art urls to be cached (spotify for example) make
> sure `gvfs` is enabled.
>
> :::code-group
>
> ```nix [<i class="devicon-nixos-plain"></i> configuration.nix]
> services.gvfs.enable = true;
> ```

1. install dependencies

   :::code-group

   ```sh [<i class="devicon-archlinux-plain"></i> Arch]
   sudo pacman -Syu meson vala valadoc gvfs json-glib gobject-introspection
   ```

   ```sh [<i class="devicon-fedora-plain"></i> Fedora]
   sudo dnf install meson vala valadoc gvfs json-glib-devel gobject-introspection-devel
   ```

   ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
   sudo apt install meson valac valadoc gvfs libjson-glib-dev gobject-introspection
   ```

   :::

2. clone repo

   ```sh
   git clone https://github.com/aylur/astal.git
   cd astal/lib/mpris
   ```

3. install

   ```sh
   meson setup build
   meson install -C build
   ```
