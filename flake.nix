{
  description = "Library and cli tool for controlling media players";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: let
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
      gvfs
      glib
      json-glib
    ];
  in {
    packages.${system} = rec {
      default = mpris;
      mpris = pkgs.stdenv.mkDerivation {
        inherit nativeBuildInputs buildInputs;
        pname = "astal-mpris";
        version = version;
        src = ./.;
        outputs = ["out" "dev"];
      };
    };

    devShells.${system} = {
      default = pkgs.mkShell {
        inherit nativeBuildInputs buildInputs;
      };
      mpris = pkgs.mkShell {
        inherit nativeBuildInputs;
        buildInputs = buildInputs ++ [
          self.packages.${system}.default
          pkgs.gjs
        ];
      };
    };
  };
}
