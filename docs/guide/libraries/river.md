# River

Library and CLI tool for monitoring the
[River Wayland Compositor](https://isaacfreund.com/software/river/).

## Usage

You can browse the [River reference](https://aylur.github.io/libastal/river).

### CLI

```sh
astal-river --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import River from "gi://AstalRiver"

const river = River.get_default()

for (const output of river.get_outputs()) {
  print(output.name)
}
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalRiver as River

river = River.get_default()

for output in river.get_outputs():
    print(output.get_name())
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local River = require("lgi").require("AstalRiver")

local river = River.River.get_default()

for _, o in ipairs(river.outputs) do
    print(o.name)
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
