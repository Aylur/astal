# Nix

## Astal

Using Astal on Nix will require you to package your project.

:::code-group

```nix [typescript.nix]
# Not documented yet
```

```nix [lua.nix]
# Not documented yet
```

```nix [python.nix]
# Not documented yet
```

```nix [vala.nix]
# Not documented yet
```

:::

## AGS

The recommended way to use AGS on NixOS is through the home-manager module.

Example content of a `flake.nix` file that contains your `homeConfigurations`.

<!--TODO: remove v2 after merge-->

:::code-group

```nix [flake.nix]
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # add ags https://github.com/Aylur/ags/pull/504
    ags.url = "github:aylur/ags/v2";
  };

  outputs = { home-manager, nixpkgs, ... }@inputs:
  let
    system = "x86_64-linux";
  in
  {
    homeConfigurations."${username}" = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs { inherit system; };

      # pass inputs as specialArgs
      extraSpecialArgs = { inherit inputs; };

      # import your home.nix
      modules = [ ./home-manager/home.nix ];
    };
  };
}
```

:::

Example content of `home.nix` file

:::code-group

```nix [home.nix]
{ inputs, pkgs, ... }:
{
  # add the home manager module
  imports = [ inputs.ags.homeManagerModules.default ];

  programs.ags = {
    enable = true;
    configDir = ../ags;

    # additional packages to add to gjs's runtime
    extraPackages = with pkgs; [
      inputs.ags.packages.${pkgs.system}.battery
      fzf
    ];
  };
}
```

:::

AGS by default only includes the core `libastal` library.
If you want to include any other [library](../libraries/references) you have to add them to `extraPackages`.
You can also add binaries which will be added to `$PATH`.

:::warning
The `configDir` option symlinks the given path to `~/.config/ags`.
If you already have your source code there leave it as `null`.
:::
