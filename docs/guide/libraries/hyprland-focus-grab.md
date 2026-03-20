# Hyprland Focus Grab

Library for making windows behave like popups using the [hyprland-focus-grab-v1](https://wayland.app/protocols/hyprland-focus-grab-v1) Wayland protocol.

## Usage

Create a `GrabContext`, add some windows to it, and set its `active` property to true to begin the grab.
During the grab, only windows added to the grab object can be interacted with. Clicking outside will cancel the click and end the grab.
The context object will emit the `cleared` signal when the grab is dismissed (usually due to clicking away).
You can then set `active` to true again to start another grab.

You can browse the
[Hyprland Focus Grab reference](https://aylur.github.io/libastal/hyprland-focus-grab).

### Library

> [!NOTE]
> This library supports both GTK 3 and 4! Make sure to import the version that matches your GTK; the following examples use GTK 4.

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import HyprlandFocusGrab from "gi://AstalHyprland?version=4.0"

const context = new HyprlandFocusGrab.GrabContext();
context.add(myWindow);
context.active = true;
```

```py [<i class="devicon-python-plain"></i> Python]
gi.require_version("AstalHyprlandFocusGrab", "4.0")
from gi.repository import AstalHyprlandFocusGrab as HyprlandFocusGrab
context = HyprlandFocusGrab.GrabContext()
context.add(my_window)
context.active = True
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local HyprlandFocusGrab = require("lgi").require("AstalHyprlandFocusGrab", "4.0")

local context = HyprlandFocusGrab.GrabContext()
context:add(my_window)
context.active = true
```

```vala [<i class="devicon-vala-plain"></i> Vala]
var context = new AstalHyprlandFocusGrab.GrabContext()
context.add(my_window)
context.active = true
```

:::

## Installation

> [!WARNING]
> You'll need to have astal-wl installed first.

1. install dependencies

    Either GTK 3 or 4 is also required.

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

2. clone repo

    ```sh
    git clone https://github.com/aylur/astal.git
    cd astal/lib/hyprland-focus-grab
    ```

3. install

    Use the `-Dgtk-version` parameter to control which GTK versions to build against.
    You can put `-Dgtk-version=3,4` to build for both.

    ```sh
    meson setup build -Dgtk-version=4
    meson install -C build
    ```
