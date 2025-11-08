# Simple Astal Bar example in TypeScript

This example shows you how to get a TypeScript+Blueprint+Sass project going.

## Dependencies

- gjs
- meson
- esbuild
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

- generate types with `ts-for-gir`

  ```sh
  # might take a while
  # also, don't worry about warning and error logs
  npx @ts-for-gir/cli generate --ignoreVersionConflicts
  ```

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

- adding new typescript files requires no additional steps
- adding new scss files requires no additional steps as long as they are imported from `style.scss`
- adding new ui (blueprint) files will also have to be listed in `meson.build` and in `gresource.xml`
