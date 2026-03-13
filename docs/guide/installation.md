# Installation

## Arch

maintainer: [@kotontrion](https://github.com/kotontrion)

```sh [Every Library]
yay -S libastal-meta
```

## Nix

maintainer: [@Aylur](https://github.com/Aylur)

Read more about it on the [nix page](./nix#astal)

## Building From Source

Most libraries will require you to follow these three steps to build and install
them. These steps are shown for each library on their individual pages.

1. Install the following dependencies

   :::code-group

   ```sh [<i class="devicon-archlinux-plain"></i> Arch]
   sudo pacman -Syu \
     meson vala valadoc gobject-introspection \
     gtk3 gtk-layer-shell \
     gtk4 gtk4-layer-shell
   ```

   ```sh [<i class="devicon-fedora-plain"></i> Fedora]
   sudo dnf install \
     meson vala valadoc gobject-introspection-devel wayland-protocols-devel \
     gtk3-devel gtk-layer-shell-devel \
     gtk4-devel gtk4-layer-shell-devel
   ```

   ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
   sudo apt install \
     meson valac valadoc gobject-introspection libgirepository1.0-dev \
     libgtk-3-dev libgtk-layer-shell-dev \
     libgtk-4-dev libgtk4-layer-shell-dev
   ```

   :::

2. Clone the repo

   ```sh
   git clone https://github.com/aylur/astal.git
   ```

3. Build and install with `meson`

   ::: code-group

   ```sh [astal-io]
   cd lib/astal/io
   meson setup build
   meson install -C build
   ```

   ```sh [astal3]
   cd lib/astal/gtk3
   meson setup build
   meson install -C build
   ```

   ```sh [astal4]
   cd lib/astal/gtk4
   meson setup build
   meson install -C build
   ```

   :::
