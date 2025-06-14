{
  mkAstalPkg,
  ...
}:
mkAstalPkg {
  pname = "astal-wl";
  src = ./.;

  libname = "wl";
  authors = "kotontrion";
  gir-suffix = "Wl";
  description = "A central wayland connection manager for the other libs.";
}
