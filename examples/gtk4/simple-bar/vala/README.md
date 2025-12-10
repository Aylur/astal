# Simple Astal Bar example in Vala

This example shows you how to get a Vala+Blueprint+Sass project going.

## Dependencies

- vala
- meson
- blueprint-compiler
- sass
- astal4
- astal-battery
- astal-wireplumber
- astak-network
- astal-mpris
- astak-power-profiles
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
  meson setup build --wipe
  meson install -C build
  simple-bar
  ```

- adding new vala files will also have to be listed in `meson.build`
- adding new scss files requires no additional steps as long as they are imported from `style.scss`
- adding new ui (blueprint) files will also have to be listed in `meson.build` and in `gresource.xml`
