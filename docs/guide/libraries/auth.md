# Auth

Library and CLI tool for authentication using
[pam](https://github.com/linux-pam/linux-pam).

## Usage

You can browse the [Auth reference](https://aylur.github.io/libastal/auth).

### CLI

```sh
astal-auth --password my-password
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Auth from "gi://AstalAuth"

Auth.Pam.authenticate("password", (_, task) => {
  try {
    AstalAuth.Pam.authenticate_finish(task)
    print("authentication sucessful")
  } catch (error) {
    print(error)
  }
})
```

```py [<i class="devicon-python-plain"></i> Python]
from gi.repository import AstalAuth as Auth

def callback(_, task) -> None:
    try:
        Auth.Pam.authenticate_finish(task)
        print("success")
    except Exception as e:
        print(e)

Auth.Pam.authenticate("password", callback)
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Auth = require("lgi").require("AstalAuth")

Auth.Pam.authenticate("password", function(_, task)
    local status, err = Auth.Pam.authenticate_finish(task)
    if err ~= nil then
        print(err)
    else
        print("success")
    end
end)
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::

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

   > [!WARNING] On NixOS you have to add `astal-auth` to `security.pam`.
   >
   > ::: code-group
   >
   > ```nix [configuration.nix]
   > { security.pam.services.astal-auth = {} }
   > ```
   >
   > :::

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
