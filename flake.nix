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
        attrs (import nixpkgs {inherit system;}));
  in {
    lib = {
      mkLuaPackage = import ./nix/lua.nix self;
    };

    packages = perSystem (pkgs: let
      mkPkg = src:
        import src {
          inherit self pkgs;
          mkAstalPkg = import ./nix/mkAstalPkg.nix pkgs;
        };
    in {
      default = self.packages.${pkgs.system}.io;
      docs = import ./docs {inherit self pkgs;};

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

      gjs = import ./lang/gjs {inherit self pkgs;};
    });

    devShells = perSystem (pkgs:
      import ./nix/devshell.nix {
        inherit pkgs self;
      });
  };
}
