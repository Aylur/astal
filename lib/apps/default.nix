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
  gir-suffix = "Apps";
  authors = "Aylur";
  description = "Application query library";
}
