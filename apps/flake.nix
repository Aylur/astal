{
  description = "Library and cli tool for querying applications";

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
      json-glib
    ];
  in {
    packages.${system} = rec {
      default = apps;
      apps = pkgs.stdenv.mkDerivation {
        inherit nativeBuildInputs buildInputs;
        pname = "astal-apps";
        version = version;
        src = ./.;
        outputs = ["out" "dev"];
      };
    };

    devShells.${system} = {
      default = pkgs.mkShell {
        inherit nativeBuildInputs buildInputs;
      };
      apps = pkgs.mkShell {
        inherit nativeBuildInputs;
        buildInputs = buildInputs ++ [
          pkgs.gjs
          self.packages.${system}.default
        ];
      };
    };
  };
}
