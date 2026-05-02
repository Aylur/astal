{
  mkAstalPkg,
  ...
}:
mkAstalPkg {
  pname = "astal-brightness";
  src = ./.;

  libname = "brightness";
  gir-suffix = "Brightness";
  authors = "brainlessbitch";
  description = "Backlight brightness control";
}
