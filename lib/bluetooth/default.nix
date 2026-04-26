{mkAstalPkg, ...}:
mkAstalPkg {
  pname = "astal-bluetooth";
  src = ./.;

  libname = "bluetooth";
  authors = "Aylur";
  name = "AstalBluetooth";
  description = "DBus proxy for bluez";
}
