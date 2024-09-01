{
  description = "Wrapper library for WirePlumber";

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
      glib
      wireplumber
      # json-glib
    ];
  in {
    packages.${system} = rec {
      default = wireplumber;
      wireplumber = pkgs.stdenv.mkDerivation {
        inherit nativeBuildInputs buildInputs;
        pname = "astal-wireplumber";
        version = version;
        src = ./.;
        outputs = ["out" "dev"];
      };
    };

    devShells.${system} = {
      default = pkgs.mkShell {
        inherit nativeBuildInputs buildInputs;
      };
      wireplumber = pkgs.mkShell {
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
