{mkAstalPkg, ...}:
mkAstalPkg {
  pname = "astal";
  src = ./.;

  libname = "io";
  gir-suffix = "IO";
  authors = "Aylur";
  description = "Astal Core library";
  repo-path = "astal/io";
  website-path = "io";
}
