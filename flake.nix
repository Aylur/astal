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
    };

    devShells.${system} = let
      inputs = with pkgs; [
        wrapGAppsHook
        gobject-introspection
        meson
        pkg-config
        ninja
        vala
        (lua.withPackages (ps: [ps.lgi]))
        gjs
      ];
    in {
      default = pkgs.mkShell {
        inherit inputs;
      };
      astal = pkgs.mkShell {
        inputs = inputs ++ [self.packages.${system}.astal];
      };
    };
  };
}
