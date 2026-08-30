# AstalWl

This library provides a framework for other libs implementing wayland protocols. It is not intended to be used directly.

## Usage

You can browse the [AstalWl reference](https://docs.astal.dev/wl).

## Installation

1. install dependencies

    :::code-group

    ```sh [<i class="devicon-archlinux-plain"></i> Arch]
    sudo pacman -Syu meson vala valadoc wayland-client
    ```
    :::

2. clone repo

    ```sh
    git clone https://github.com/aylur/astal.git
    cd astal/lib/wl
    ```

3. install

    ```sh
    meson setup build
    meson install -C build
    ```
