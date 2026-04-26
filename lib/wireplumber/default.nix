{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-wireplumber";
  src = ./.;
  packages = [pkgs.wireplumber];

  libname = "wireplumber";
  authors = "kotontrion";
  name = "AstalWp";
  description = "Wrapper library over the wireplumber API";
  dependencies = ["WP-0.5"];
}
