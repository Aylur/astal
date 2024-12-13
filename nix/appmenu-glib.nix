{
  stdenv,
  fetchFromGitLab,
  pkg-config,
  meson,
  ninja,
  gobject-introspection,
  vala,
  glib,
}: let
  vala-panel-appmenu = fetchFromGitLab {
    owner = "vala-panel-project";
    repo = "vala-panel-appmenu";
    rev = "24.05";
    hash = "sha256-8GWauw7r3zKhvGF2TNOI8GDVctUFDhtG/Vy1cNUpsVo=";
  };
in
  stdenv.mkDerivation {
    pname = "appmenu-glib-translator";
    version = "24.05";

    src = "${vala-panel-appmenu}/subprojects/appmenu-glib-translator";

    buildInputs = [
      glib
    ];

    nativeBuildInputs = [
      gobject-introspection
      meson
      pkg-config
      ninja
      vala
    ];
  }
