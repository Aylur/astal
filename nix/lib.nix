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
  } @ args:
    stdenv.mkDerivation {
      inherit pname version;
      src = path {
        name = "${pname}-${version}";
        src = cleanSource args.src;
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
        args.buildInputs
      ];

      outputs = ["out" "dev"];
    };
in {
  inherit getVersionFromFile mkAstalPkg;
}
