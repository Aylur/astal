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
  name = "AstalAuth";
  authors = "kotontrion";
  description = "Authentication using pam";
}
