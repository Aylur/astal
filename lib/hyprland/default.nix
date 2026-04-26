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
  name = "AstalHyprland";
  description = "IPC client for Hyprland";
}
