{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    version = builtins.replaceStrings ["\n"] [""] (builtins.readFile ./version);
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};

    lib = name: src: inputs:
      pkgs.stdenv.mkDerivation {
        nativeBuildInputs = with pkgs; [
          wrapGAppsHook
          gobject-introspection
          meson
          pkg-config
          ninja
          vala
          wayland
        ];
        buildInputs = [pkgs.glib] ++ inputs;
        pname = name;
        version = version;
        src = src;
        outputs = ["out" "dev"];
      };
  in {
    packages.${system} = rec {
      default = astal;
      astal = with pkgs; lib "astal" ./core [gtk3 gtk-layer-shell];
      apps = with pkgs; lib "astal-apps" ./lib/apps [json-glib];
      auth = with pkgs; lib "astal-auth" ./lib/auth [pam];
      battery = lib "astal-battery" ./lib/battery [];
      bluetooth = lib "astal-bluetooth" ./lib/bluetooth [];
      hyprland = with pkgs; lib "astal-hyprland" ./lib/hyprland [json-glib];
      mpris = with pkgs; lib "astal-mpris" ./lib/mpris [gvfs json-glib];
      network = with pkgs; lib "astal-network" ./lib/network [networkmanager];
      notifd = with pkgs; lib "astal-notifd" ./lib/notifd [json-glib gdk-pixbuf];
      powerprofiles = with pkgs; lib "astal-power-profiles" ./lib/powerprofiles [json-glib];
      river = with pkgs; lib "astal-river" ./lib/river [json-glib];
      tray = with pkgs; lib "astal-tray" ./lib/tray [gtk3 gdk-pixbuf libdbusmenu-gtk3 json-glib];
      wireplumber = lib "astal-wireplumber" ./lib/wireplumber [pkgs.wireplumber];
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
