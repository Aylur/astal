{
  pkgs,
  self,
  ...
}:
pkgs.stdenvNoCC.mkDerivation {
  src = ./.;
  name = "astal-gjs";
  nativeBuildInputs = [
    pkgs.meson
    pkgs.ninja
    pkgs.pkg-config
    self.packages.${pkgs.stdenv.hostPlatform.system}.io
    self.packages.${pkgs.stdenv.hostPlatform.system}.astal3
  ];
}
