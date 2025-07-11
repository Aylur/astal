# Notifd

A
[notification daemon](https://specifications.freedesktop.org/notification-spec/latest/)
implementation as a library and CLI tool.

## How it works

The first instantiation of the
[Notifd](https://aylur.github.io/libastal/notifd/class.Notifd.html) class will
become the daemon and every subsequent instantiation will queue up to act as the
daemon and will act as a client in the meantime. This means this library can be
used throughout multiple processes.

## Usage

You can browse the [Notifd reference](https://aylur.github.io/libastal/notifd).

### CLI

```sh
astal-notifd --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Notifd from "gi://AstalNotifd"

const notifd = Notifd.get_default()

notifd.connect("notified", (_, id) => {
  const n = notifd.get_notification(id)
  print(n.summary, n.body)
})
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalNotifd as Notifd

notifd = Notifd.get_default()

def on_notified(_, id):
    n = notifd.get_notification(id)
    print(n.get_body(), n.get_body())

notifd.connect("notified", on_notified)
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Notifd = require("lgi").require("AstalNotifd")

local notifd = Notifd.get_default()

notifd.on_notified = function(_, id)
    local n = notifd.get_notification(id)
    print(n.body, n.summary)
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
   sudo pacman -Syu meson vala valadoc gdk-pixbuf2 json-glib gobject-introspection
   ```

   ```sh [<i class="devicon-fedora-plain"></i> Fedora]
   sudo dnf install meson vala valadoc gdk-pixbuf2-devel json-glib-devel gobject-introspection-devel
   ```

   ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
   sudo apt install meson valac valadoc libgdk-pixbuf-2.0-dev libjson-glib-dev gobject-introspection
   ```

   :::

2. clone repo

   ```sh
   git clone https://github.com/aylur/astal.git
   cd astal/lib/notifd
   ```

3. install

   ```sh
   meson setup build
   meson install -C build
   ```
