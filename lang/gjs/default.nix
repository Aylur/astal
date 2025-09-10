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
    self.packages.io
    self.packages.astal3
  ];
}
