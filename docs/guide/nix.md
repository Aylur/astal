# Nix

Using Astal on Nix will require you to write a derivation for your project. You
can either copy and build off of these example flakes or you can incorporate the
derivations into your existing flake/configuration.

> [!WARNING]
>
> This page expects you to know what a nix derivation is and how to use/install
> them.

## Installing libraries versus installing executables

In case you did not know already, you can't install libraries globally on Nix as
you would with regular package managers like `pacman`, `dnf` or `apt`. You have
to write a derivation for your projects like you would for any other program. If
you try to install a library through `home.packages` or
`environment.systemPackages` don't expect it to be picked up from runtimes.

However, if you want to use the CLI tool that comes with some of the libraries
you have to **also** install them through `home.packages` or
`environment.systemPackages` alongside your derivations.

## TypeScript

Using [esbuild](https://esbuild.github.io/) as the bundler.

:::details In most cases you will want to use meson

Meson is useful to also install data files such as assets.

In which case you can omit `installPhase` and just include meson in
`nativeBuildInputs`:

```nix
{
  nativeBuildInputs = [
    pkgs.wrapGAppsHook3
    pkgs.gobject-introspection
    pkgs.esbuild
    pkgs.meson # [!code ++]
    pkgs.ninja # [!code ++]
    pkgs.pkg-config # [!code ++]
  ];
}
```

:::

:::code-group

```nix [<i class="devicon-nixos-plain"></i> flake.nix]
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    astal = {
      url = "github:aylur/astal";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    astal,
  }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.${system}.default = pkgs.stdenvNoCC.mkDerivation { # [!code focus:29]
      name = "my-shell";
      src = ./.;

      nativeBuildInputs = [
        pkgs.wrapGAppsHook3
        pkgs.gobject-introspection
        pkgs.esbuild
      ];

      buildInputs = [
        pkgs.gjs
        pkgs.glib
        pkgs.gtk4
        astal.packages.${system}.io
        astal.packages.${system}.astal4
      ];

      installPhase = ''
        mkdir -p $out/bin

        esbuild \
          --bundle src/app.js \
          --outfile=$out/bin/my-shell \
          --format=esm \
          --sourcemap=inline \
          --external:gi://\*
      '';
    };
  };
}
```

:::

## Lua

:::code-group

```nix [<i class="devicon-nixos-plain"></i> flake.nix]
# Not yet documented
```

:::

## Python

:::code-group

```nix [<i class="devicon-nixos-plain"></i> flake.nix]
# Not yet documented
```

:::

## Vala

Keep in mind that this is just the nix derivation and you still have to use some
build tool like meson.

:::code-group

```nix [<i class="devicon-nixos-plain"></i> flake.nix]
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    astal = {
      url = "github:aylur/astal";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, astal }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.${system}.default = pkgs.stdenv.mkDerivation { # [!code focus:19]
      name = "my-shell";
      src = ./.;

      nativeBuildInputs = with pkgs; [
        meson
        ninja
        pkg-config
        vala
        gobject-introspection
      ];

      buildInputs = [
        astal.packages.${system}.io
        astal.packages.${system}.astal3
        astal.packages.${system}.battery
        # add extra packages
      ];
    };
  };
}
```

:::
