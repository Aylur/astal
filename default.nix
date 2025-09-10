{
  system ? builtins.currentSystem,
  nixpkgs ? <nixpkgs>,
  pkgs ? import nixpkgs { inherit system; },
  lib ? pkgs.lib,
}:
lib.fix (self: {
  src = ./.;

  packages =
    let
      mkPkg =
        src:
        import src {
          inherit self pkgs;
          mkAstalPkg = import ./nix/mkAstalPkg.nix pkgs;
        };
    in
    {
      default = self.packages.io;
      docs = import ./docs { inherit self pkgs; };

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
    };

  shells = import ./nix/devshell.nix {
    inherit self pkgs;
  };
})
