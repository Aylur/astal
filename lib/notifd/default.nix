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
  name = "AstalNotifd";
  description = "Notification daemon library";
}
