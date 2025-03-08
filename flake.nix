{
  inputs = {
    systems.url = "github:nix-systems/default-linux";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = {
    self,
    systems,
    nixpkgs,
  }: let
    perSystem = attrs:
      nixpkgs.lib.genAttrs (import systems) (system:
        attrs (import nixpkgs {
          inherit system;
          overlays = [self.overlays.default];
        }));
  in {
    lib = {
      mkLuaPackage = import ./nix/lua.nix self;
    };

    packages = perSystem (pkgs: pkgs.astal // {default = pkgs.astal.io;});

    overlays = {
      astal = final: prev: {
        astal = let
          mkPkg = src:
            import src {
              pkgs = final;
              mkAstalPkg = import ./nix/mkAstalPkg.nix final;
            };
        in {
          docs = import ./docs final;

          io = mkPkg ./lib/astal/io;
          astal3 = mkPkg ./lib/astal/gtk3;
          astal4 = mkPkg ./lib/astal/gtk4;
          apps = mkPkg ./lib/apps;
          auth = mkPkg ./lib/auth;
          battery = mkPkg ./lib/battery;
          bluetooth = mkPkg ./lib/bluetooth;
          cava = mkPkg ./lib/cava;
          greet = mkPkg ./lib/greet;
          hyprland = mkPkg ./lib/hyprland;
          mpris = mkPkg ./lib/mpris;
          network = mkPkg ./lib/network;
          notifd = mkPkg ./lib/notifd;
          powerprofiles = mkPkg ./lib/powerprofiles;
          river = mkPkg ./lib/river;
          tray = mkPkg ./lib/tray;
          wireplumber = mkPkg ./lib/wireplumber;

          gjs = import ./lang/gjs final;
        };
      };
      default = self.overlays.astal;
    };

    devShells = perSystem (pkgs:
      import ./nix/devshell.nix pkgs);

    formatter = perSystem (pkgs: pkgs.alejandra);
  };
}
