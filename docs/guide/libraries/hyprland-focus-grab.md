# Hyprland Focus Grab

Library for making windows modal using the [hyprland-focus-grab-v1](https://wayland.app/protocols/hyprland-focus-grab-v1) Wayland protocol.

## Usage

You can browse the
[Hyprland Focus Grab reference](https://aylur.github.io/libastal/hyprland-focus-grab).


### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import HyprlandFocusGrab from "gi://AstalHyprland"
// TODO
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalHyprlandFocusGrab as HyprlandFocusGrab
# TODO
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local HyprlandFocusGrab = require("lgi").require("AstalHyprlandFocusGrab")
-- TODO
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// TODO
```

:::

## Installation

this library depends on astal-wl, which is another astal library. How do we rectify this

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

2. clone repo

    ```sh
    git clone https://github.com/aylur/astal.git
    cd astal/lib/hyprland-focus-grab
    ```

3. install

    ```sh
    meson setup build
    meson install -C build
    ```
