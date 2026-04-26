{mkAstalPkg, ...}:
mkAstalPkg {
  pname = "quarrel";
  src = ./.;

  libname = "quarrel";
  name = "Quarrel";
  authors = "Aylur";
  description = "Command line argument parser.";
}
