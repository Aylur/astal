# Auth

Library and CLI tool for authentication using [pam](https://github.com/linux-pam/linux-pam).

## Installation

1. install dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson pam gobject-introspection
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson pam-devel gobject-introspection-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
# Not yet documented
```

:::

::: warning On NixOS you have to add `astal-auth` to `security.pam`.
::: code-group

```nix [configuration.nix]
{
  security.pam.services.astal-auth = {}
}
```

:::

2. clone repo

```sh
git clone https://github.com/aylur/astal.git
cd astal/lib/auth
```

3. install

```sh
meson setup build
meson install -C build
```

:::tip
Most distros recommend manual installs in `/usr/local`,
which is what `meson` defaults to. If you want to install to `/usr`
instead which most package managers do, set the `prefix` option:

```sh
meson setup --prefix /usr build
```

:::

## Usage

You can browse the [Auth reference](https://aylur.github.io/libastal/auth).

### CLI

```sh
astal-auth --password my-password
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Auth from "gi://AstalAuth";
import Gio from "gi://Gio";

Gio._promisify(Auth.Pam, "authenticate");

await Auth.Pam.authenticate("password")
    .then(_ => print("authentication sucessful"))
    .catch(logError);
```

```py [<i class="devicon-python-plain"></i> Python]
import gi

gi.require_version("AstalAuth", "0.1")

from gi.repository import AstalAuth 

def callback(_, task) -> None:
    try:
        AstalAuth.Pam.authenticate_finish(task)
        print("success")
    except Exception as e:
        print(e)

AstalAuth.Pam.authenticate("password", callback)
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local lgi = require("lgi")

local AstalAuth = lgi.require("AstalAuth", "0.1")

AstalAuth.Pam.authenticate("password", function(_, task)
	local status, err = AstalAuth.Pam.authenticate_finish(task)
	if status == 0 then
		print("success")
	else
		print(err)
	end
end)
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::
