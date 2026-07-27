# Workspace

Library for monitoring and manipulating the compositor's workspaces,
based on the [ext-workspace-v1](https://wayland.app/protocols/ext-workspace-v1) protocol.

## Usage

You can browse the [Workspace reference](https://docs.astal.dev/workspace).

### CLI

```sh
astal-workspace --help
```

### Library

The AstalWorkspace library is initialized asynchronously; therefore, all lists
(e.g. `workspaces`) are initially empty.
This is usually not an issue. When you bind to these values, your widget will
receive updates as soon as the compositor finishes sending the data.
However, this means that you won't get the correct data when accessing the library's properties at
the top level. You can listen for the "updated" signal to accomodate this.

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Workspace from "gi://AstalWorkspace"

const ws = Workspace.get_default()

for (const workspace of ws.workspaces) {
    print(workspace.name);
}
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalWorkspace as Workspace

ws = Workspace.get_default()

for workspace in ws.get_workspaces():
    print(workspace.get_name())
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Workspace = require("lgi").require("AstalWorkspace")

local ws = Workspace.get_default()

for _, workspace in ipairs(ws.workspaces) do
    print(workspace.name)
end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
var ws = AstalWorkspace.get_default();

foreach (var workspace in ws.workspaces) {
    print(workspace.name);
}
```

:::

## Installation

1. install dependencies

    Note that the library depends on AstalWl, which must be installed before this one.

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
    cd astal/lib/workspace
    ```

3. install

    ```sh
    meson setup build
    meson install -C build
    ```
