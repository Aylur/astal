{
  description = "Notification daemon library and cli tool";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
  let
    version = builtins.replaceStrings ["\n"] [""] (builtins.readFile ./version);
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };

    nativeBuildInputs = with pkgs; [
      gobject-introspection
      meson
      pkg-config
      ninja
      vala
    ];

    buildInputs = with pkgs; [
      glib
      gdk-pixbuf
      json-glib
    ];
  in {
    packages.${system} = rec {
      default = notifd;
      notifd = pkgs.stdenv.mkDerivation {
        inherit nativeBuildInputs buildInputs;
        pname = "notifd";
        version = version;
        src = ./.;
        outputs = ["out" "dev"];
      };
    };

    devShells.${system} = {
      default = pkgs.mkShell {
        inherit nativeBuildInputs buildInputs;
      };
    };
  };
}
