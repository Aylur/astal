{
  mkAstalPkg,
  pkgs,
  ...
}: let
  wl-vapi-gen = pkgs.stdenv.mkDerivation {
    pname = "wl-vapi-gen";
    version = "v0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "kotontrion";
      repo = "wl-vapi-gen";
      rev = "v0.1.0";
      hash = "sha256-//WEvytcGeQYYC/CdUDuT86p0PPtc+iiCawvC941Ux8=";
    };
 
    nativeBuildInputs = with pkgs; [
      meson
      ninja
      python3
    ];
  };
in
mkAstalPkg {
  pname = "astal-wl";
  src = ./.;
  packages = [wl-vapi-gen];

  libname = "wl";
  authors = "kotontrion";
  gir-suffix = "Wl";
  description = "A central wayland connection manager for the other libs.";
}
