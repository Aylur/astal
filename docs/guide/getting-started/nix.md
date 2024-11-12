---
next:
  link: '/guide/getting-started/supported-languages'
  text: 'Supported Languages'
---
# Nix

Using Astal on Nix will require you to package your project.

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
