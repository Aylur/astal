---
next:
  link: '/guide/getting-started/supported-languages'
  text: 'Supported Languages'
---
# Nix

Using Astal on Nix will require you to write a derivation for your project.
You can either copy and build off of these example flakes or you can
incorporate the derivations into your existing flake/configuration.

## Q: "How do I install Astal on Nix?"

:::details See answer
A: <span style="font-size: 1.2em; font-weight: bold;">You don't.</span>

You can't install libraries globally on Nix as you would with regular
package managers like `pacman`, `dnf` or `apt`. You have to write a
derivation for your projects like you would for any other program.
:::

## TypeScript

Instructions for using [AGS](https://aylur.github.io/ags/) as the bundler
are now located on [the Nix page on the AGS Wiki](https://aylur.github.io/ags/guide/nix.html#bundle-and-devshell).


The following flake exposes a devShell with an astalGjsBundler command
(shebang usage: `#!/usr/bin/env -S astalBundler --run`)
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
  outputs = {nixpkgs, astal, self, ...}:
  let
    inherit (pkgs) lib;
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    astalPkgs = astal.packages.${system};

    extraPackages = with pkgs; [
      glib

      # Remove the packages related to the gtk version you're not using
      gtk3
      gtk-layer-shell
      astalPkgs.astal3

      gtk4
      gtk4-layer-shell
      astalPkgs.astal4

      # Change and add packages as desired
      astalPkgs.greet
    ];

  in {
    packages.${system} = {
      bundler = pkgs.writeShellApplication {
        name = "astalGjsBundler";
        runtimeInputs = with pkgs; [esbuild gjs];
        # Remove the following line if you're not using gtk4
        runtimeEnv.LD_PRELOAD = "${pkgs.gtk4-layer-shell}/lib/libgtk4-layer-shell.so";
        derivationArgs = {
          # Use wrapGAppsHook4 if you're using gtk4
          nativeBuildInputs = with pkgs; [wrapGAppsHook3 gobject-introspection];
          buildInputs = extraPackages;
        };
        text = ''
          if [[ $# == 0 ]]; then
            echo "[astalGjsBundler] You must at least provide an entrypoint in positional arguments"
            exit 1
          elif [[ $# == 1 ]]; then
            entrypoint="$1";
          elif [[ $# == 2 ]]; then
            entrypoint="$2"
            [[ $1 == "--run" ]] && run=true
          else
            echo "[astalGjsBundler] More than 2 arguments were provided"
            echo "[astalGjsBundler] They were: $*"
            echo "[astalGjsBundler] Exiting..."
            exit 1
          fi

          # Inherits the environmental value or defaults to /tmp/my-shell.js
          outfile="''${outfile:-/tmp/my-shell.js}";
          # Same as above, except it defaults to "warning"
          logLevel="''${logLevel:-warning}"
          # If previously set, preserve the old value. Otherwise, set to false
          run="''${run:-false}"

          esbuild \
            --log-level="''${loglevel:-warning}" \
            --external:{console,systemtem,cairo,gettext,"file://*","gi://*","resource://*"} \
            --format=esm --target=es2022 \
            --alias:astal="${astalPkgs.gjs}/share/astal/gjs" \
            --bundle "$entrypoint" --outfile="$outfile"

          if [[ $run == "true" ]]; then gjs -m "$outfile"; fi
        '';
      };
      default = pkgs.stdenvNoCC.mkDerivation rec {
        name = "my-shell";
        version = "0.1.0";
        src = lib.cleanSource path/to/source; # Should contain an app.ts or another entrypoint

        nativeBuildInputs = with pkgs; [
          gobject-introspection
          wrapGAppsHook3 # Use wrapGAppsHook4 if needed
          self.packages.${system}.bundler
        ];
        buildInputs = [pkgs.gjs] ++ extraPackages;

        # If app.ts isn't your entry point, modify this accordingly
        buildPhase = "astalGjsBundler app.ts";

        installPhase = ''
          mkdir -p $out/bin

          shebang='#!/usr/bin/env gjs -m'
          firstLine="$(head -n 1 $outfile)"

          if [[ $firstline == \#!* ]] && [[ $firstline != $shebang ]]; then
            # If the file already contains a shebang, create $out/bin/$${name}
            # after making the `$shebang` variable the first line
            {
              echo $shebang;
              tail -n +2 $outfile;
            } > $out/bin/${name}
          else
            # Creates that file after making the $shebang the first line
            echo $shebang | cat - $outfile > $out/bin/${name}
          fi

          chmod +x $out/bin/${name}
        '';

        # Remove this entire phase if not using gtk4
        preFixup = ''
          gappsWrapperArgs+=(--set LD_PRELOAD "${pkgs.gtk4-layer-shell}/lib/libgtk4-layer-shell.so")
        '';

        loglevel = "error";
        outfile = "built.js";

        meta.mainProgram = name;
      };
    };
    devShells.${system}.default = pkgs.mkShell {
      # Use wrapGAppsHook4 if needed
      nativeBuildInputs = with pkgs; [wrapGAppsHook3 gobject-introspection];
      buildInputs = extraPackages;
      packages = with pkgs; [self.packages.${system}.bundler esbuild gjs];

      # Remove if not using gtk4
      LD_PRELOAD = "${pkgs.gtk4-layer-shell}/lib/libgtk4-layer-shell.so";
    };
  };
}
```

:::

:::tip
You can use any other bundler too, like `bun`
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
