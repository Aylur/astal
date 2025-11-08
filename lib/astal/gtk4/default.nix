{
  mkAstalPkg,
  pkgs,
  self,
}:
mkAstalPkg {
  pname = "astal4";
  src = ./.;
  packages = [
    self.packages.${pkgs.stdenv.hostPlatform.system}.io
    pkgs.gtk4
    pkgs.gtk4-layer-shell
  ];

  libname = "astal4";
  gir-suffix = "";
  authors = "Aylur";
  description = "Astal GTK4 widget library";
  dependencies = ["AstalIO-0.1" "Gtk-4.0"];
  repo-path = "astal/gtk4";
}
