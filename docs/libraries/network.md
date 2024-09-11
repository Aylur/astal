# Network

Wrapper library over [networkmanager](https://networkmanager.dev/) to better integrate with Astal.

## Installation

1. install dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson vala networkmanager gobject-introspection
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson gcc valac NetworkManager gobject-introspection-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
sudo apt install meson valac network-manager-dev gobject-introspection
```

:::

2. clone repo

```sh
git clone https://github.com/aylur/astal.git
cd astal/lib/network
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

You can browse the [Network reference](https://aylur.github.io/libastal/network).

### CLI

There is no CLI for this library, use the one provided by networkmanager.

```sh
nmcli --help
```

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import Network from "gi://AstalNetwork";

const network = Network.get_default()

console.log(network.wifi.ssid)
```

```py [<i class="devicon-python-plain"></i> Python]
# Not yet documented
```

```lua [<i class="devicon-lua-plain"></i> Lua]
-- Not yet documented
```

```vala [<i class="devicon-vala-plain"></i> Vala]
// Not yet documented
```

:::
