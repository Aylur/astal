{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-apps";
  src = ./.;
  packages = [pkgs.json-glib];
  libname = "apps";
  name = "AstalApps";
  authors = "Aylur";
  description = "Application query library";
}
