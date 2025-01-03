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
  gir-suffix = "PowerProfiles";
  description = "DBus proxy for upowerd profiles";
}
