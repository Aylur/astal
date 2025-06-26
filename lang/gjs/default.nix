pkgs:
pkgs.stdenvNoCC.mkDerivation {
  src = ./.;
  name = "astal-gjs";
  nativeBuildInputs = [
    pkgs.meson
    pkgs.ninja
    pkgs.pkg-config
    pkgs.astal.io
    pkgs.astal.astal3
  ];
}
