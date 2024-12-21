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
  gir-suffix = "Greet";
  description = "IPC client for greetd";
}
