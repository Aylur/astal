{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-river";
  src = ./.;
  packages = [pkgs.json-glib];

  libname = "river";
  authors = "kotontrion";
  gir-suffix = "River";
  description = "IPC client for River";

  postUnpack = ''
    rm -rf $sourceRoot/subprojects
    mkdir -p $sourceRoot/subprojects
    cp -r --remove-destination ${../wayland-glib} $sourceRoot/subprojects/wayland-glib
  '';
}
