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
  name = "AstalMpris";
  description = "Control mpris players";
}
