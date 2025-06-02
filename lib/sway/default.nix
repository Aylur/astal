{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-sway";
  src = ./.;
  packages = [pkgs.json-glib];

  libname = "sway";
  authors = "Noname";
  gir-suffix = "Sway";
  description = "IPC client for Sway";
}
