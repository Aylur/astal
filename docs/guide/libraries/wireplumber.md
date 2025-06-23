# Wire Plumber

Wrapper library over
[wireplumber](https://pipewire.pages.freedesktop.org/wireplumber/) to better
integrate with Astal.

## Usage

You can browse the
[Wireplumber reference](https://aylur.github.io/libastal/wireplumber).

### CLI

There is no CLI for this library, use the one provided by wireplumber.

```sh
wpctl --help
```

### Library

The AstalWp library is initialized asynchronously; therefore, all lists (e.g.
`audio.speakers`) are initially empty, and the properties have default values.
This is usually not an issue. When you bind to these values, your widget will
receive updates as soon as the library has loaded the data. However, this means
that you won't get the correct data when accessing the library's properties at
the top level. To accommodate this, it emits the `ready` signal once, as soon as
the initial data has been loaded.

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Wp from "gi://AstalWp"

const wp = Wp.get_default()
const default_speaker = wp.audio.default_speaker;

wp.connect("ready", () => {
    print(default_speaker.volume)
}
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalWp as Wp

audio = Wp.get_default().get_audio()

print(audio.get_default_speaker().get_volume())
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Wp = require("lgi").require("AstalWp")

local audio = Wp.get_default().audio

print(audio.default_speaker.volume)
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::

## Installation

1. install dependencies

   :::code-group

   ```sh [<i class="devicon-archlinux-plain"></i> Arch]
   sudo pacman -Syu meson vala valadoc wireplumber gobject-introspection
   ```

   ```sh [<i class="devicon-fedora-plain"></i> Fedora]
   sudo dnf install meson vala valadoc wireplumber-devel gobject-introspection-devel
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
