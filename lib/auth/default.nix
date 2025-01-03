{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-auth";
  src = ./.;
  packages = [pkgs.pam];

  libname = "auth";
  gir-suffix = "Auth";
  authors = "kotontrion";
  description = "Authentication using pam";
}
