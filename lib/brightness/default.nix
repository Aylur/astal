{ mkAstalPkg, ... }:
mkAstalPkg {
  pname = "astal-brightness";
  src = ./.;

  libname = "brightness";
  authors = "brainlessbitch";
  gir-suffix = "Brightness";
  description = "Backlight brightness control";
}
