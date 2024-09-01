{
  description = "Library and cli for the StatusNotifierItem protocol";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    version = builtins.replaceStrings ["\n"] [""] (builtins.readFile ./version);
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};

    nativeBuildInputs = with pkgs; [
      gobject-introspection
      meson
      pkg-config
      ninja
      vala
    ];

    buildInputs = with pkgs; [
      gtk3
      glib
      gdk-pixbuf
      libdbusmenu-gtk3
      json-glib
    ];
  in {
    packages.${system} = rec {
      default = tray;
      tray = pkgs.stdenv.mkDerivation {
        inherit nativeBuildInputs buildInputs;
        pname = "astal-tray";
        version = version;
        src = ./.;
        outputs = ["out" "dev"];
      };
    };

    devShells.${system} = {
      default = pkgs.mkShell {
        inherit nativeBuildInputs buildInputs;
      };
      tray = pkgs.mkShell {
        inherit nativeBuildInputs;
        buildInputs =
          buildInputs
          ++ [
            self.packages.${system}.default
            pkgs.gjs
          ];
      };
    };
  };
}
