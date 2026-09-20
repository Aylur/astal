# AstalWl

This library provides a framework for other libs implementing wayland protocols.
The core (AstalWl itself) is not intended to be used directly,
however there also exists a helper library to map GTK 4 objects to AstalWl named AstalWl4.

## Usage

You can browse the [AstalWl reference](https://docs.astal.dev/wl/wl).

For GTK 4 support, you can browse the [AstalWl4 reference](https://docs.astal.dev/wl/wl4).

## Installation

1. install dependencies

    :::code-group

    ```sh [<i class="devicon-archlinux-plain"></i> Arch]
    sudo pacman -Syu meson vala valadoc wayland-client
    ```

    ```sh [<i class="devicon-fedora-plain"></i> Fedora]
    sudo dnf install meson vala valadoc wayland-devel wayland-protocols-devel gobject-introspection-devel
    ```

    ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
    sudo apt install meson valac valadoc libwayland-dev wayland-protocols gobject-introspection
    ```

    :::

    For AstalWl4, you will also need GTK 4.

    :::code-group

    ```sh [<i class="devicon-archlinux-plain"></i> Arch]
    sudo pacman -Syu gtk4
    ```

    ```sh [<i class="devicon-fedora-plain"></i> Fedora]
    sudo dnf install gtk4-devel
    ```

    ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
    sudo apt install libgtk-4-dev
    ```

    :::

2. clone repo

    ```sh
    git clone https://github.com/aylur/astal.git
    cd astal/lib/wl/wl
    ```

3. install

    ```sh
    meson setup build
    meson install -C build
    ```

    For AstalWl4, make sure the base AstalWl is installed first and then do the install step in the directory `lib/wl/wl4`.
