{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    astal.url = "github:aylur/astal";
  };

  outputs = {
    self,
    nixpkgs,
    astal,
  }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    packages.${system} = {
      default = pkgs.stdenv.mkDerivation {
        name = "simple-bar";
        src = ./.;

        nativeBuildInputs = with pkgs; [
          meson
          ninja
          pkg-config
          vala
          gobject-introspection
          dart-sass
        ];

        buildInputs = [
          astal.packages.${system}.io
          astal.packages.${system}.astal3
          astal.packages.${system}.battery
          astal.packages.${system}.wireplumber
          astal.packages.${system}.network
          astal.packages.${system}.tray
          astal.packages.${system}.mpris
          astal.packages.${system}.hyprland
        ];
      };
    };
  };
}
