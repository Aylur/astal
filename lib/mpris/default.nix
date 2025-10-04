{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-mpris";
  src = ./.;
  packages = [pkgs.gvfs pkgs.json-glib];

  libname = "mpris";
  authors = "Aylur";
  gir-suffix = "Mpris";
  description = "Control mpris players";
}
