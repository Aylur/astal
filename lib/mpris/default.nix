{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-mpris";
  src = ./.;
  packages = with pkgs; [gvfs json-glib];

  libname = "mpris";
  authors = "Aylur";
  gir-suffix = "Mpris";
  description = "Control mpris players";
}
