{
  self,
  pkgs,
}: let
  toTOML = (pkgs.formats.toml {}).generate;

  genRefForPkg = {
    name,
    pkg,
    outPath,
    metaData,
  }: let
    data = toTOML name metaData;
    output = self.packages.${pkgs.system}.${pkg}.dev;
  in ''
    mkdir -p $out/${outPath}
    cat ${urlmap} > urlmap.js
    gi-docgen generate -C ${data} ${output}/share/gir-1.0/${name}-0.1.gir
    cp -r ${name}-0.1/* $out/${outPath}
  '';

  genLib = name: namespace: description: {
    authors ? "Aylur",
    dependencies ? {},
    out ? "libastal/${name}",
  }:
    genRefForPkg {
      name = "Astal${namespace}";
      pkg = name;
      outPath = out;
      metaData = {
        library = {
          inherit description authors;
          license = "LGPL-2.1";
          browse_url = "https://github.com/aylur/astal";
          repository_url = "https://github.com/aylur/aylur.git";
          website_url = "https://aylur.github.io/astal";
          dependencies = ["GObject-2.0"] ++ (builtins.attrNames dependencies);
        };

        extra.urlmap_file = "urlmap.js";
        dependencies = dependencies // dependency;
      };
    };

  dependency = {
    "GObject-2.0" = {
      name = "GObject";
      description = "The base type system library";
      docs_url = "https://developer.gnome.org/gobject/stable";
    };
  };

  urlmap = pkgs.writeText "urlmap" ''
    baseURLs = ${builtins.toJSON [
      ["GLib" "https://docs.gtk.org/glib/"]
      ["GObject" "https://docs.gtk.org/gobject/"]
      ["Gio" "https://docs.gtk.org/gio/"]
      ["Gdk" "https://docs.gtk.org/gdk4/"]
      ["Gtk" "https://docs.gtk.org/gtk4/"]
      ["GdkPixbuf" "https://docs.gtk.org/gdk-pixbuf/"]
    ]}
  '';
in
  pkgs.stdenvNoCC.mkDerivation {
    name = "library-reference";
    src = ./.;

    nativeBuildInputs = with pkgs; [
      gi-docgen
      glib
      json-glib
      gobject-introspection
      gtk3
      gtk-layer-shell
      gdk-pixbuf
      libdbusmenu-gtk3
      wireplumber
      networkmanager
    ];

    installPhase = ''
      runHook preInstall
      ${genLib "astal" "" "Astal core library" {out = "libastal";}}
      ${genLib "apps" "Apps" "Application query library" {}}
      ${genLib "auth" "Auth" "Authentication using pam" {authors = "kotontrion";}}
      ${genLib "battery" "Battery" "DBus proxy for upowerd devices" {}}
      ${genLib "bluetooth" "Bluetooth" "DBus proxy for bluez" {}}
      ${genLib "hyprland" "Hyprland" "IPC client for Hyprland" {}}
      ${genLib "mpris" "Mpris" "Control mpris players" {}}
      ${genLib "network" "Network" "NetworkManager wrapper library" {}}
      ${genLib "notifd" "Notifd" "Notification daemon library" {}}
      ${genLib "powerprofiles" "PowerProfiles" "DBus proxy for upowerd profiles" {}}
      ${genLib "river" "River" "IPC client for River" {authors = "kotontrion";}}
      ${genLib "tray" "Tray" "StatusNotifierItem implementation" {authors = "kotontrion";}}
      ${genLib "wireplumber" "Wp" "Wrapper library over the wireplumber API" {authors = "kotontrion";}}
      runHook postInstall
    '';
  }
