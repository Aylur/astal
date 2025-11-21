{
  mkAstalPkg,
  pkgs,
  ...
}: let
  vala-panel-appmenu = pkgs.fetchFromGitLab {
    owner = "vala-panel-project";
    repo = "vala-panel-appmenu";
    rev = "25.04";
    hash = "sha256-v5J3nwViNiSKRPdJr+lhNUdKaPG82fShPDlnmix5tlY=";
  };

  appmenu-glib-translator = pkgs.stdenv.mkDerivation {
    pname = "appmenu-glib-translator";
    version = "25.04";

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
