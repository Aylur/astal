{mkAstalPkg, ...}:
mkAstalPkg {
  pname = "astal-bluetooth";
  src = ./.;

  libname = "bluetooth";
  authors = "Aylur";
  gir-suffix = "Bluetooth";
  description = "DBus proxy for bluez";
}
