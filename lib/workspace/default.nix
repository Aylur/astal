{
  mkAstalPkg,
  pkgs,
  self,
}: let
  astal = self.packages.${pkgs.stdenv.hostPlatform.system};
in
  mkAstalPkg {
    pname = "astal-workspace";
    src = ./.;
    packages = [
      astal.quarrel
      astal.wl
      pkgs.json-glib
    ];

    libname = "workspace";
    name = "AstalWorkspace";
    authors = "Mabi";
    description = "Astal Wayland Workspace";
    dependencies = ["AstalWl-0.1"];
    repo-path = "astal/workspace";
  }
