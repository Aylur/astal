{
  mkAstalPkg,
  pkgs,
  ...
}: let
  vala-panel-appmenu = pkgs.fetchFromGitLab {
    owner = "vala-panel-project";
    repo = "vala-panel-appmenu";
    rev = "24.05";
    hash = "sha256-8GWauw7r3zKhvGF2TNOI8GDVctUFDhtG/Vy1cNUpsVo=";
  };

  appmenu-glib-translator = pkgs.stdenv.mkDerivation {
    pname = "appmenu-glib-translator";
    version = "24.05";

    src = "${vala-panel-appmenu}/subprojects/appmenu-glib-translator";

    buildInputs = with pkgs; [
      glib
    ];

    nativeBuildInputs = with pkgs; [
      gobject-introspection
      meson
      pkg-config
      ninja
      vala
    ];
  };
in
  mkAstalPkg {
    pname = "astal-tray";
    src = ./.;
    packages = [pkgs.json-glib appmenu-glib-translator];

    libname = "tray";
    authors = "kotontrion";
    gir-suffix = "Tray";
    description = "StatusNotifierItem implementation";
  }
