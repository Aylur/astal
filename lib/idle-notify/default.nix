{
  mkAstalPkg,
  pkgs,
  self,
}: let
  astal = self.packages.${pkgs.stdenv.hostPlatform.system};
in
  mkAstalPkg {
    pname = "astal-idle-notify";
    src = ./.;
    packages = [
      astal.wl
    ];

    libname = "idle-notify";
    name = "AstalIdleNotify";
    authors = "kotontrion";
    description = "Astal Wayland Idle Notifiction Library";
    dependencies = ["AstalWl-0.1"];
    repo-path = "astal/idle-notify";
  }
