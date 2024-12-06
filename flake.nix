{
  outputs = {
    self,
    nixpkgs,
  }: let
    inherit (builtins) replaceStrings readFile;
    readVer = file: replaceStrings ["\n"] [""] (readFile file);

    system = "x86_64-linux"; # TODO: other architectures
    pkgs = nixpkgs.legacyPackages.${system};

    mkPkg = name: description: src: inputs:
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

        meta = {inherit description;};
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

      io = mkPkg "astal" "Astal Core library" ./lib/astal/io [];
      astal3 = mkPkg "astal" "Astal GTK3 widget library" ./lib/astal/gtk3 [self.packages.${system}.io gtk3 gtk-layer-shell];
      astal4 = mkPkg "astal" "Astal GTK4 widget library" ./lib/astal/gtk4 [self.packages.${system}.io gtk4 gtk4-layer-shell];
      apps = mkPkg "astal-apps" "Application query library" ./lib/apps [json-glib];
      auth = mkPkg "astal-auth" "Authentication using pam" ./lib/auth [pam];
      battery = mkPkg "astal-battery" "DBus proxy for upowerd devices" ./lib/battery [json-glib];
      bluetooth = mkPkg "astal-bluetooth" "DBus proxy for bluez" ./lib/bluetooth [];
      cava = mkPkg "astal-cava" "Audio visualization library using cava" ./lib/cava [(pkgs.callPackage ./nix/libcava.nix {})];
      greet = mkPkg "astal-greet" "IPC client for greetd" ./lib/greet [json-glib];
      hyprland = mkPkg "astal-hyprland" "IPC client for Hyprland" ./lib/hyprland [json-glib];
      mpris = mkPkg "astal-mpris" "Control mpris players" ./lib/mpris [gvfs json-glib];
      network = mkPkg "astal-network" "NetworkManager wrapper library" ./lib/network [networkmanager];
      notifd = mkPkg "astal-notifd" "Notification daemon library" ./lib/notifd [json-glib gdk-pixbuf];
      powerprofiles = mkPkg "astal-power-profiles" "DBus proxy for upowerd profiles" ./lib/powerprofiles [json-glib];
      river = mkPkg "astal-river" "IPC client for River" ./lib/river [json-glib];
      tray = mkPkg "astal-tray" "StatusNotifierItem implementation" ./lib/tray [gtk3 gdk-pixbuf libdbusmenu-gtk3 json-glib];
      wireplumber = mkPkg "astal-wireplumber" "Wrapper library over the wireplumber API" ./lib/wireplumber [wireplumber];

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

        meta.description = "gjs bindings for AstalIO, Astal3, and Astal4";
      };
    };
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
}
