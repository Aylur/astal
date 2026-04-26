{mkAstalPkg, ...}:
mkAstalPkg {
  pname = "astal";
  src = ./.;
  libname = "io";
  name = "IO";
  authors = "Aylur";
  description = "Astal Core library";
  repo-path = "astal/io";
  website-path = "io";
}
