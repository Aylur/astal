{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-network";
  src = ./.;
  packages = [pkgs.networkmanager];

  libname = "network";
  authors = "Aylur";
  gir-suffix = "Network";
  description = "NetworkManager wrapper library";
  dependencies = ["NM-1.0"];
}
