{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-battery";
  src = ./.;
  packages = [pkgs.json-glib];

  libname = "battery";
  authors = "Aylur";
  name = "AstalBattery";
  description = "DBus proxy for upowerd devices";
}
