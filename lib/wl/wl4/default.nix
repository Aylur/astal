{
  mkAstalPkg,
  pkgs,
  self,
}:
mkAstalPkg {
  pname = "wl4";
  src = ./.;
  packages = [
    self.packages.${pkgs.stdenv.hostPlatform.system}.wl
    pkgs.gtk4
  ];

    libname = "wl4";
    authors = "Mabi";
    name = "AstalWl4";
    description = "A thin util lib to match GTK4 objects to AstalWl ones";
    repo-path = "wl/wl4";
  }


