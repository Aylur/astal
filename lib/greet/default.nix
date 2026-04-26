{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-greet";
  src = ./.;
  packages = [pkgs.json-glib];

  libname = "greet";
  authors = "Aylur";
  name = "AstalGreet";
  description = "IPC client for greetd";
}
