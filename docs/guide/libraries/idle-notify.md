# IdleNotify

Library implementing the `ext-idle-notifier-v1` wayland protocol.

## Usage

You can browse the [IdleNotify reference](https://docs.astal.dev/idle-notify).

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import IdleNotify from "gi://AstalIdleNotify"

const notifier = IdleNotify.get_default()

const notif = notifier.get_input_idle_notification(1000)

notif.connect("idled", () => print("idled"))
notif.connect("resumed", () => print("resumed"))
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalIdleNotify as IdleNotify

notifier = IdleNotify.get_default()

notif = notifier.get_input_idle_notification(1000)

notif.connect("idled", lambda _: print("idled"))
notif.connect("resumed", lambda _: print("resumed"))
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local IdleNotify = require("lgi").require("AstalIdleNotify")

local notifier = IdleNotify.get_default()

local notif = notifier:get_input_idle_notification(1000)

notif.on_idled = function() print("idled") end
notif.on_resumed = function() print("resumed") end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
var notifier = AstalIdleNotify.get_default();

var notif = notifier.get_input_idle_notification(1000);

notif.idled.connect(() => print("idled\n"));
notif.resumed.connect(() => print("resumed\n"));
```

:::

## Installation

1. install dependencies

    This lib does depend on AstalWl, which needs to be installed first.

    :::code-group

    ```sh [<i class="devicon-archlinux-plain"></i> Arch]
    sudo pacman -Syu meson vala valadoc wayland-client gobject-introspection
    ```

    ```sh [<i class="devicon-fedora-plain"></i> Fedora]
    sudo dnf install meson vala valadoc wayland-devel wayland-protocols-devel gobject-introspection-devel
    ```

    ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
    sudo apt install meson valac valadoc libwayland-dev wayland-protocols gobject-introspection
    ```

    :::

2. clone repo

    ```sh
    git clone https://github.com/aylur/astal.git
    cd astal/lib/idle-notify
    ```

3. install

    ```sh
    meson setup build
    meson install -C build
    ```
