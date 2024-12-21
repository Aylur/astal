{
  mkAstalPkg,
  pkgs,
  ...
}:
mkAstalPkg {
  pname = "astal-notifd";
  src = ./.;
  packages = with pkgs; [json-glib gdk-pixbuf];

  libname = "notifd";
  authors = "Aylur";
  gir-suffix = "Notifd";
  description = "Notification daemon library";
}
