# Installation

## Arch

maintainer: [@kotontrion](https://github.com/kotontrion)

:::code-group

```sh [Core Library]
yay -S libastal-io-git libastal-git
```

```sh [Every Library]
yay -S libastal-meta
```

:::

## Nix

maintainer: [@Aylur](https://github.com/Aylur)

Read more about it on the [nix page](./nix#astal)

## Building From Source

1. Install the following dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson vala valadoc gtk3 gtk-layer-shell gobject-introspection
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson vala valadoc gtk3-devel gtk-layer-shell-devel gobject-introspection-devel wayland-protocols-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
sudo apt install meson valac valadoc libgtk-3-dev libgtk-layer-shell-dev gobject-introspection libgirepository1.0-dev
```

:::

2. Clone the repo

```sh
git clone https://github.com/aylur/astal.git /tmp/astal
```

3. Build and install with `meson`

- astal-io

```sh
cd /tmp/astal/lib/astal/io
meson setup build
meson install -C build
```

- astal3

```sh
cd /tmp/astal/lib/astal/gtk3
meson setup build
meson install -C build
```
