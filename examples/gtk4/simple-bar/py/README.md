# Simple Astal Bar example in Python

This example shows you how to get a Python+Blueprint+Sass project going.

## Dependencies

- python3
- pygobject
- meson
- blueprint-compiler
- sass
- astal4
- astal-battery
- astal-wireplumber
- astak-network
- astal-mpris
- astak-powerprofiles
- astal-tray
- astal-bluetooth

## How to use

> [!NOTE]
> If you are on Nix, there is an example flake included
> otherwise feel free to `rm flake.nix`

- developing

    ```sh
    meson setup build --wipe --prefix "$(pwd)/result"
    meson install -C build
    ./result/bin/simple-bar
    ```

- installing

    ```sh
    meson setup build --wipe --prefix /usr
    meson install -C build
    simple-bar
    ```

- adding new python files requires no additional steps
- adding new scss files requires no additional steps as long as they are imported from `main.scss`
- adding new ui (blueprint) files will also have to be listed in `meson.build` and in `gresource.xml`
