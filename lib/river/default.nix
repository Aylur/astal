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
}
