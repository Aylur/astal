{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    astal = {
      url = "github:aylur/astal";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    astal,
  }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

    python = pkgs.python3.withPackages (ps: [
      ps.pygobject3
    ]);

    nativeBuildInputs = with pkgs; [
      meson
      ninja
      pkg-config
      gobject-introspection
      wrapGAppsHook4
      blueprint-compiler
      dart-sass
    ];

    astalPackages = with astal.packages.${system}; [
      io
      astal4
      battery
      wireplumber
      network
      mpris
      powerprofiles
      tray
      bluetooth
    ];
  in {
    packages.${system}.default = pkgs.stdenv.mkDerivation {
      name = "simple-bar";
      src = ./.;
      inherit nativeBuildInputs;
      buildInputs = astalPackages ++ [python];
    };

    devShells.${system}.default = pkgs.mkShell {
      packages = nativeBuildInputs ++ astalPackages ++ [python];
    };
  };
}
