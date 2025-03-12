---
next:
  link: '/guide/getting-started/supported-languages'
  text: 'Supported Languages'
---
# Nix

Using Astal on Nix will require you to write a derivation for your project.
You can either copy and build off of these example flakes or you can
incorporate the derivations into your existing flake/configuration.

## Installing libraries versus installing executables

In case you did not know already,
you can't install libraries globally on Nix as you would with regular
package managers like `pacman`, `dnf` or `apt`. You have to write a
derivation for your projects like you would for any other program.
If you try to install a library through `home.packages` or `environment.systemPackages`
don't expect it to be picked up from runtimes.

However, if you want to use the CLI tool that comes with some of the libraries
you have to **also** install them through `home.packages` or `environment.systemPackages`
alongside your derivations.

### Astal CLI

The core library also comes with a CLI tool that you can use to send
requests to your app.

:::code-group

```nix [nixos]
environment.systemPackages = [inputs.astal.packages.${system}.default];
```

```nix [home-manager]
home.packages = [inputs.astal.packages.${system}.default];
```

:::

```sh [astal cli]
astal --list # list running instances
```

## TypeScript

Using [AGS](https://aylur.github.io/ags/) as the bundler.

:::code-group

```nix [<i class="devicon-nixos-plain"></i> flake.nix]
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    astal = {
      url = "github:aylur/astal";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ags = {
      url = "github:aylur/ags";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, astal, ags }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.${system}. default = pkgs.stdenvNoCC.mkDerivation rec {
      name = "my-shell";
      src = ./.;

      nativeBuildInputs = [
        ags.packages.${system}.default
        pkgs.wrapGAppsHook
        pkgs.gobject-introspection
      ];

      buildInputs = with astal.packages.${system}; [
        astal3
        io
        # any other package
      ];

      installPhase = ''
        mkdir -p $out/bin
        ags bundle app.ts $out/bin/${name}
      '';
    };
  };
}
```

:::

:::tip
You can use any other bundler too like `esbuild`
which is what `ags` uses under the hood.
:::

## Lua

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
    packages.${system}.default = astal.lib.mkLuaPackage {
      inherit pkgs;
      name = "my-shell"; # how to name the executable
      src = ./path/to/project; # should contain init.lua

      # add extra glib packages or binaries
      extraPackages = [
        astal.packages.${system}.battery
        pkgs.dart-sass
      ];
    };
  };
}
```

:::

## Python

:::code-group

```nix [<i class="devicon-nixos-plain"></i> flake.nix]
# Not documented yet
```

:::

## Vala

Keep in mind that this is just the nix derivation
and you still have to use some build tool like meson.

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
    packages.${system}.default = pkgs.stdenv.mkDerivation {
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
