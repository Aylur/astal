# Greet

Library and CLI tool for sending requests to
[greetd](https://sr.ht/~kennylevinsen/greetd/).

## Usage

You can browse the [Greet reference](https://aylur.github.io/libastal/greet).

### CLI

```sh
astal-greet --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Greet from "gi://AstalGreet"

Greet.login("username", "password", "compositor", (_, res) => {
  try {
    Greet.login_finish(res)
  } catch (err) {
    printerr(err)
  }
})
```

```py [<i class="devicon-python-plain"></i> Python]
# Not yet documented

```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Greet = require("lgi").require("AstalGreet")

Greet.login("username", "password", "compositor", function (_, res)
    local err = Greet.login_finish(res)
    if err ~= nil then
        print(err)
    end
end)
```

```vala [<i class="devicon-vala-plain"></i> Vala]
try {
    yield AstalGreet.login("username", "password", "compositor");
} catch (Error err) {
    printerr(err.message);
}
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

   Although `greetd` is not a direct build dependency, it should be
   self-explanatory that the daemon is required to be available at runtime.

   :::

2. clone repo

   ```sh
   git clone https://github.com/aylur/astal.git
   cd astal/lib/greet
   ```

3. install

   ```sh
   meson setup build
   meson install -C build
   ```
