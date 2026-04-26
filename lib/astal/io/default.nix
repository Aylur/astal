{mkAstalPkg, ...}:
mkAstalPkg {
  pname = "astal";
  src = ./.;
  libname = "io";
  name = "AstalIO";
  authors = "Aylur";
  description = "Astal Core library";
  repo-path = "astal/io";
  website-path = "io";
}
