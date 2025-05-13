# Sway

Library and CLI tool for monitoring the Sway socket.

## Usage

You can browse the [sway reference](https://aylur.github.io/libastal/sway).

## Installation

1. install dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson vala json-glib gobject-introspection
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson gcc valac json-glib-devel gobject-introspection-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
sudo apt install meson valac libjson-glib-dev gobject-introspection
```

:::

2. clone repo

```sh
git clone https://github.com/aylur/astal.git
cd astal/lib/sway
```

3. install

```sh
meson setup build
meson install -C build
```
