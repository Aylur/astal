{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-hyprland";
  src = ./.;
  packages = [pkgs.json-glib];

  libname = "hyprland";
  authors = "Aylur";
  gir-suffix = "Hyprland";
  description = "IPC client for Hyprland";
}
