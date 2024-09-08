{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    inherit (builtins) replaceStrings readFile;

    version = replaceStrings ["\n"] [""] (readFile ./version);
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};

    mkPkg = name: src: inputs:
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
        propagatedBuildInputs = [pkgs.glib] ++ inputs;
        pname = name;
        version = version;
        src = src;
        outputs = ["out" "dev"];
      };
  in {
    packages.${system} = with pkgs; {
      docs = import ./docs {inherit self pkgs;};
      default = self.packages.${system}.astal;

      astal = mkPkg "astal" ./core [gtk3 gtk-layer-shell];
      apps = mkPkg "astal-apps" ./lib/apps [json-glib];
      auth = mkPkg "astal-auth" ./lib/auth [pam];
      battery = mkPkg "astal-battery" ./lib/battery [];
      bluetooth = mkPkg "astal-bluetooth" ./lib/bluetooth [];
      hyprland = mkPkg "astal-hyprland" ./lib/hyprland [json-glib];
      mpris = mkPkg "astal-mpris" ./lib/mpris [gvfs json-glib];
      network = mkPkg "astal-network" ./lib/network [networkmanager];
      notifd = mkPkg "astal-notifd" ./lib/notifd [json-glib gdk-pixbuf];
      powerprofiles = mkPkg "astal-power-profiles" ./lib/powerprofiles [json-glib];
      river = mkPkg "astal-river" ./lib/river [json-glib];
      tray = mkPkg "astal-tray" ./lib/tray [gtk3 gdk-pixbuf libdbusmenu-gtk3 json-glib];
      wireplumber = mkPkg "astal-wireplumber" ./lib/wireplumber [wireplumber];
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
        (python3.withPackages (ps: [ps.pygobject3 ps.pygobject-stubs]))
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
