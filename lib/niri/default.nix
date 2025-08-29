{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-niri";
  src = ./.;
  packages = [pkgs.json-glib];

  libname = "niri";
  authors = "sameoldlab";
  gir-suffix = "Niri";
  description = "IPC client for Niri";
}
