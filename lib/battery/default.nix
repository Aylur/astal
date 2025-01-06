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
  gir-suffix = "Battery";
  description = "DBus proxy for upowerd devices";
}
