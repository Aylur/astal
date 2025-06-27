{
  mkAstalPkg,
  pkgs,
  self,
  ...
}:
mkAstalPkg {
  pname = "astal-river";
  src = ./.;
  packages = [
    self.packages.${pkgs.system}.wl
    pkgs.json-glib
  ];

  libname = "river";
  authors = "kotontrion";
  gir-suffix = "River";
  description = "IPC client for River";
}
