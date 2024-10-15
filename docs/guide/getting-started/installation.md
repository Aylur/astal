# Installation

## Nix

maintainer: [@Aylur](https://github.com/Aylur)

Read more about it on the [nix page](./nix#astal)

## Arch

maintainer: [@kotontrion](https://github.com/kotontrion)

<!--TODO: fix aur package names-->

:::code-group

```sh [Core Library]
yay -S libastal-git
```

```sh [Every Library]
yay -S libastal-meta
```

:::

## Bulding libastal from source

1. Clone the repo

```sh
git clone https://github.com/aylur/astal.git
```

2. Install the following dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu meson vala gtk3 gtk-layer-shell gobject-introspection
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install meson gcc valac gtk3-devel gtk-layer-shell-devel gobject-introspection-devel
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
sudo apt install meson valac libgtk-3-dev libgtk-layer-shell-dev gobject-introspection libgirepository1.0-dev
```

:::

3. Build and install with `meson`

- astal-io

```sh
cd lib/astal/io
meson setup --prefix /usr build
meson install -C build
```

- astal3

```sh
cd lib/astal/gtk3
meson setup --prefix /usr build
meson install -C build
```
