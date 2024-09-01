{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    astalLib = pkgs.callPackage ./nix/lib.nix {inherit version;};
    inherit (astalLib) getVersionFromFile mkAstalPkg;

    version = getVersionFromFile ./version;
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
  in {
    packages.${system} = let
      inherit (pkgs) callPackage;
      inherit (pkgs) gtk3 gtk-layer-shell json-glib pam gvfs networkmanager gdk-pixbuf libdbusmenu-gtk3;
    in {
      default = self.packages.${system}.astal;
      docs = callPackage ./docs {
        flakePkgs = self.packages.${system};
      };

      astal = mkAstalPkg {
        pname = "astal";
        src = ./core;
        buildInputs = [gtk3 gtk-layer-shell];
      };

      apps = mkAstalPkg {
        pname = "astal-apps";
        src = ./lib/apps;
        buildInputs = [json-glib];
      };

      auth = mkAstalPkg {
        pname = "astal-auth";
        src = ./lib/auth;
        buildInputs = [pam];
      };

      battery = mkAstalPkg {
        pname = "astal-battery";
        src = ./lib/battery;
      };

      bluetooth = mkAstalPkg {
        pname = "astal-bluetooth";
        src = ./lib/bluetooth;
      };

      hyprland = mkAstalPkg {
        pname = "astal-hyprland";
        src = ./lib/hyprland;
        buildInputs = [json-glib];
      };

      mpris = mkAstalPkg {
        pname = "astal-mpris";
        src = ./lib/mpris;
        buildInputs = [gvfs json-glib];
      };

      network = mkAstalPkg {
        pname = "astal-network";
        src = ./lib/network;
        buildInputs = [networkmanager];
      };

      notifd = mkAstalPkg {
        pname = "astal-notifd";
        src = ./lib/notifd;
        buildInputs = [json-glib gdk-pixbuf];
      };

      powerprofiles = mkAstalPkg {
        pname = "astal-power-profiles";
        src = ./lib/powerprofiles;
        buildInputs = [json-glib];
      };

      river = mkAstalPkg {
        pname = "astal-river";
        src = ./lib/river;
        buildInputs = [json-glib];
      };

      tray = mkAstalPkg {
        pname = "astal-tray";
        src = ./lib/tray;
        buildInputs = [gtk3 gdk-pixbuf libdbusmenu-gtk3 json-glib];
      };

      wireplumber = mkAstalPkg {
        pname = "astal-wireplumber";
        src = ./lib/wireplumber;
        buildInputs = [pkgs.wireplumber];
      };
    };

    devShells.${system} = let
      buildInputs = with pkgs; [
        wrapGAppsHook
        gobject-introspection
        meson
        pkg-config
        ninja
        vala
        gtk3
        gtk-layer-shell
        json-glib
        pam
        gvfs
        networkmanager
        gdk-pixbuf
        wireplumber
        libdbusmenu-gtk3
        wayland
        (lua.withPackages (ps: [ps.lgi]))
        gjs
      ];
    in {
      default = pkgs.mkShell {
        inherit buildInputs;
      };
      astal = pkgs.mkShell {
        buildInputs = buildInputs ++ (builtins.attrValues self.packages.${system});
      };
    };
  };
}
