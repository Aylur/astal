{
  self,
  pkgs,
}: let
  inherit (builtins) replaceStrings readFile;
  readVer = file: replaceStrings ["\n"] [""] (readFile file);

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

  genLib = name: namespace: {
    description,
    version,
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
          version = readVer version;
          license = "LGPL-2.1";
          browse_url = "https://github.com/aylur/astal";
          repository_url = "https://github.com/aylur/aylur.git";
          website_url = "https://aylur.github.io/astal";
          dependencies = ["GObject-2.0"] ++ (builtins.attrNames dependencies);
        };

        extra.urlmap_file = "urlmap.js";
        dependencies = {inherit (dependency) "GObject-2.0";} // dependencies;
      };
    };

  dependency = {
    "GObject-2.0" = {
      name = "GObject";
      description = "The base type system library";
      docs_url = "https://docs.gtk.org/gobject/";
    };
    "Gtk-3.0" = {
      name = "Gtk";
      description = "The GTK toolkit";
      docs_url = "https://docs.gtk.org/gtk3/";
    };
    "NM-1.0" = {
      name = "NetworkManager";
      description = "The standard Linux network configuration tool suite";
      docs_url = "https://networkmanager.dev/docs/libnm/latest/";
    };
    "WP-0.5" = {
      name = "WirePlumber";
      description = "Modular session/policy manager for PipeWire";
      docs_url = "https://pipewire.pages.freedesktop.org/wireplumber/";
    };
  };

  urlmap = pkgs.writeText "urlmap" ''
    baseURLs = ${builtins.toJSON [
      ["GLib" "https://docs.gtk.org/glib/"]
      ["GObject" "https://docs.gtk.org/gobject/"]
      ["Gio" "https://docs.gtk.org/gio/"]
      ["Gdk" "https://docs.gtk.org/gdk3/"]
      ["Gtk" "https://docs.gtk.org/gtk3/"]
      ["GdkPixbuf" "https://docs.gtk.org/gdk-pixbuf/"]

      # FIXME: these are not gi-docgen generated, therefore links are broken
      ["NM" "https://networkmanager.dev/docs/libnm/latest/"]
      ["WP" "https://pipewire.pages.freedesktop.org/wireplumber/"]
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
      ${genLib "astal" "" "Astal core library" {
        out = "libastal";
        description = "Astal core library";
        version = ../core/version;
        dependencies = {inherit (dependency) "Gtk-3.0";};
      }}
      ${genLib "apps" "Apps" {
        description = "Application query library";
        version = ../lib/apps/version;
      }}
      ${genLib "auth" "Auth" {
        authors = "kotontrion";
        description = "Authentication using pam";
        version = ../lib/auth/version;
      }}
      ${genLib "battery" "Battery" {
        description = "DBus proxy for upowerd devices";
        version = ../lib/battery/version;
      }}
      ${genLib "bluetooth" "Bluetooth" {
        description = "DBus proxy for bluez";
        version = ../lib/bluetooth/version;
      }}
      ${genLib "hyprland" "Hyprland" {
        description = "IPC client for Hyprland";
        version = ../lib/hyprland/version;
      }}
      ${genLib "mpris" "Mpris" {
        description = "Control mpris players";
        version = ../lib/mpris/version;
      }}
      ${genLib "network" "Network" {
        description = "NetworkManager wrapper library";
        version = ../lib/network/version;
        dependencies = {inherit (dependency) "NM-1.0";}; # FIXME: why does this not work?
      }}
      ${genLib "notifd" "Notifd" {
        description = "Notification daemon library";
        version = ../lib/notifd/version;
      }}
      ${genLib "powerprofiles" "PowerProfiles" {
        description = "DBus proxy for upowerd profiles";
        version = ../lib/powerprofiles/version;
      }}
      ${genLib "river" "River" {
        description = "IPC client for River";
        version = ../lib/river/version;
        authors = "kotontrion";
      }}
      ${genLib "tray" "Tray" {
        description = "StatusNotifierItem implementation";
        version = ../lib/tray/version;
        authors = "kotontrion";
      }}
      ${genLib "wireplumber" "Wp" {
        description = "Wrapper library over the wireplumber API";
        version = ../lib/wireplumber/version;
        authors = "kotontrion";
        dependencies = {inherit (dependency) "WP-0.5";}; # FIXME: why does this not work?
      }}
      runHook postInstall
    '';
  }
