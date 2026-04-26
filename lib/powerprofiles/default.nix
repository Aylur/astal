{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-powerprofiles";
  src = ./.;
  packages = [pkgs.json-glib];

  libname = "powerprofiles";
  authors = "Aylur";
  name = "AstalPowerProfiles";
  description = "DBus proxy for upowerd profiles";
}
