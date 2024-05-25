{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    version = builtins.replaceStrings ["\n"] [""] (builtins.readFile ./version);
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};

    nativeBuildInputs = with pkgs; [
      wrapGAppsHook
      gobject-introspection
      meson
      pkg-config
      ninja
      vala
    ];

    buildInputs = with pkgs; [
      glib
      gtk3
      gtk-layer-shell
    ];
  in {
    packages.${system} = rec {
      default = astal;
      astal = pkgs.stdenv.mkDerivation {
        inherit nativeBuildInputs buildInputs;
        pname = "astal";
        version = version;
        src = ./.;
        outputs = ["out" "dev"];
      };
    };

    devShells.${system} = let
      inputs = with pkgs;
        buildInputs
        ++ [
          (lua.withPackages (ps: [ps.lgi]))
          (python3.withPackages (ps: [ps.pygobject3]))
          gjs
          deno
          nodejs
        ];
    in {
      default = pkgs.mkShell {
        inherit nativeBuildInputs;
        buildInputs = inputs;
      };
      astal = pkgs.mkShell {
        inherit nativeBuildInputs;
        buildInputs =
          inputs
          ++ [
            self.packages.${system}.astal
            pkgs.playerctl # FIXME: just for demo
          ];
      };
    };
  };
}
