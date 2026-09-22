# Brightness

Library and CLI tool for reading and controlling backlight and LED brightness
devices exposed through sysfs.

## Usage

You can browse the [Brightness reference](https://docs.astal.dev/brightness).

### CLI

```sh
astal-brightness --help
```

List available devices, set the guessed primary screen brightness, or monitor
brightness changes:

```sh
astal-brightness list --pretty
astal-brightness set 50%
astal-brightness set +10%
astal-brightness monitor
```

Use `--subsystem leds` and `--name` when targeting a specific LED device, such
as a keyboard backlight.

### Library

The singleton exposes `backlights` and `leds` device lists. It also exposes
`screen` and `keyboard` proxy devices, which are guessed from the available
backlight and LED devices.

`brightness` is a normalized value from `0` to `1`; `real_brightness` is the raw
value reported by sysfs.

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Brightness from "gi://AstalBrightness"

const brightness = Brightness.get_default()

print(brightness.screen.brightness)
brightness.screen.brightness = 0.5
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalBrightness as Brightness

brightness = Brightness.get_default()
screen = brightness.get_screen()

print(screen.get_brightness())
screen.set_brightness(0.5)
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Brightness = require("lgi").require("AstalBrightness")

local brightness = Brightness.get_default()

print(brightness.screen.brightness)
brightness.screen.brightness = 0.5
```

```vala [<i class="devicon-vala-plain"></i> Vala]
var brightness = AstalBrightness.get_default();

print(@"$(brightness.screen.brightness)\n");
brightness.screen.brightness = 0.5f;
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

    The CLI target also depends on [libquarrel](https://docs.astal.dev/quarrel).
    Install Quarrel first, or configure with `-Dcli=false` if you only need the
    library.

    The default backend uses `systemd-logind` to write brightness values. To
    install udev rules instead, configure with `-Dbrightness-backend=UDEV`.

    :::

2. clone repo

    ```sh
    git clone https://github.com/aylur/astal.git
    cd astal/lib/brightness
    ```

3. install

    ```sh
    meson setup build
    meson install -C build
    ```
