{
  version,
  lib,
  stdenv,
  glib,
  wrapGAppsHook,
  gobject-introspection,
  meson,
  pkg-config,
  ninja,
  vala,
  wayland,
  ...
}: let
  inherit (builtins) path;
  inherit (lib.sources) cleanSource;
  inherit (lib.lists) concatLists;
  inherit (lib.strings) replaceStrings readFile;

  getVersionFromFile = file: (replaceStrings ["\n"] [""] (readFile file));

  mkAstalPkg = {
    pname,
    src,
    buildInputs ? [],
  }:
    stdenv.mkDerivation {
      inherit pname version;
      src = path {
        name = "${pname}-${version}";
        path = cleanSource src;
      };

      nativeBuildInputs = [
        wrapGAppsHook
        gobject-introspection
        meson
        pkg-config
        ninja
        vala
        wayland
      ];

      buildInputs = concatLists [
        [glib]
        buildInputs
      ];

      outputs = ["out" "dev"];
    };
in {
  inherit getVersionFromFile mkAstalPkg;
}
