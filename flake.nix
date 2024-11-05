{
  outputs = {
    self,
    nixpkgs,
  }: let
    inherit (builtins) replaceStrings readFile;
    readVer = file: replaceStrings ["\n"] [""] (readFile file);

    system = "x86_64-linux"; # TODO: other architectures
    pkgs = nixpkgs.legacyPackages.${system};

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
          wayland-scanner
          python3
        ];
        propagatedBuildInputs = [pkgs.glib] ++ inputs;
        pname = name;
        version = readVer "${src}/version";
        src = src;
        postUnpack = ''
          cp --remove-destination ${./lib/gir.py} $sourceRoot/gir.py
        '';
        outputs = ["out" "dev"];
      };
  in {
    devShells.${system} = import ./nix/devshell.nix {
      inherit self pkgs;
    };

    lib = {
      mkLuaPackage = import ./nix/lua.nix {
        inherit pkgs;
        astal = self;
      };
    };

    packages.${system} = with pkgs; {
      docs = import ./docs {inherit self pkgs;};
      default = self.packages.${system}.io;

      io = mkPkg "astal" ./lib/astal/io [];
      astal3 = mkPkg "astal" ./lib/astal/gtk3 [self.packages.${system}.io gtk3 gtk-layer-shell];
      astal4 = mkPkg "astal" ./lib/astal/gtk4 [self.packages.${system}.io gtk4 gtk4-layer-shell];
      apps = mkPkg "astal-apps" ./lib/apps [json-glib];
      auth = mkPkg "astal-auth" ./lib/auth [pam];
      battery = mkPkg "astal-battery" ./lib/battery [json-glib];
      bluetooth = mkPkg "astal-bluetooth" ./lib/bluetooth [];
      cava = mkPkg "astal-cava" ./lib/cava [(pkgs.callPackage ./nix/libcava.nix {})];
      greet = mkPkg "astal-greet" ./lib/greet [json-glib];
      hyprland = mkPkg "astal-hyprland" ./lib/hyprland [json-glib];
      mpris = mkPkg "astal-mpris" ./lib/mpris [gvfs json-glib];
      network = mkPkg "astal-network" ./lib/network [networkmanager];
      notifd = mkPkg "astal-notifd" ./lib/notifd [json-glib gdk-pixbuf];
      powerprofiles = mkPkg "astal-power-profiles" ./lib/powerprofiles [json-glib];
      river = mkPkg "astal-river" ./lib/river [json-glib];
      tray = mkPkg "astal-tray" ./lib/tray [gtk3 gdk-pixbuf libdbusmenu-gtk3 json-glib];
      wireplumber = mkPkg "astal-wireplumber" ./lib/wireplumber [wireplumber];

      gjs = pkgs.stdenvNoCC.mkDerivation {
        src = ./lang/gjs;
        name = "astal-gjs";
        nativeBuildInputs = [
          meson
          ninja
          pkg-config
          self.packages.${system}.io
          self.packages.${system}.astal3
        ];
      };
    };
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
}
