{
  self,
  pkgs,
}: let
  inherit (builtins) replaceStrings readFile;
  readVer = file: replaceStrings ["\n"] [""] (readFile file);

  toTOML = (pkgs.formats.toml {}).generate;

  docgen = pkgs.gi-docgen.overrideAttrs {
    patches = [../nix/doc/gi-docgen.patch];
  };

  genLib = {
    flakepkg,
    gir,
    version,
    description,
    api-ver ? "0.1",
    authors ? "Aylur",
    dependencies ? {},
    out ? "libastal/${flakepkg}",
    browse ? flakepkg,
    website ? flakepkg,
  }: let
    name = "Astal${gir}-${api-ver}";
    src = self.packages.${pkgs.system}.${flakepkg}.dev;

    data = toTOML gir {
      library = {
        inherit description authors;
        version = readVer version;
        license = "LGPL-2.1";
        browse_url = "https://github.com/Aylur/astal/tree/main/lib/${browse}";
        repository_url = "https://github.com/aylur/aylur.git";
        website_url = "https://aylur.github.io/astal/guide/libraries/${website}";
        dependencies = ["GObject-2.0"] ++ (builtins.attrNames dependencies);
      };

      extra.urlmap_file = "urlmap.js";
      dependencies = {inherit (dependency) "GObject-2.0";} // dependencies;
    };
  in ''
    mkdir -p $out/${out}
    cat ${urlmap} > urlmap.js
    gi-docgen generate -C ${data} ${src}/share/gir-1.0/${name}.gir
    cp -r ${name}/* $out/${out}
  '';

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
    "Gtk-4.0" = {
      name = "Gtk";
      description = "The GTK toolkit";
      docs_url = "https://docs.gtk.org/gtk4/";
    };
    "AstalIO-0.1" = {
      name = "AstalIO";
      description = "Astal Core library";
      docs_url = "https://aylur.github.io/libastal/io";
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
      ["AstalIO" "https://aylur.github.io/libastal/io"]

      # FIXME: these are not gi-docgen generated, therefore links are broken
      ["NM" "https://networkmanager.dev/docs/libnm/latest/"]
      ["WP" "https://pipewire.pages.freedesktop.org/wireplumber/"]
    ]}
  '';
in
  pkgs.stdenvNoCC.mkDerivation {
    name = "reference";
    src = ./.;

    nativeBuildInputs = with pkgs; [
      docgen
      glib
      json-glib
      gobject-introspection
      gtk3
      gtk4
      gtk-layer-shell
      gtk4-layer-shell
      gdk-pixbuf
      libdbusmenu-gtk3
      wireplumber
      networkmanager
      self.packages.${system}.io
    ];

    installPhase = ''
      runHook preInstall
      ${genLib {
        flakepkg = "io";
        gir = "IO";
        api-ver = "0.1";
        browse = "astal/io";
        description = "Astal Core library";
        version = ../lib/astal/io/version;
      }}
      ${genLib {
        flakepkg = "astal3";
        gir = "";
        api-ver = "3.0";
        browse = "astal/gtk3";
        description = "Astal GTK3 widget library";
        version = ../lib/astal/gtk3/version;
        dependencies = {inherit (dependency) "AstalIO-0.1" "Gtk-3.0";};
      }}
      ${genLib {
        flakepkg = "astal4";
        gir = "";
        api-ver = "4.0";
        browse = "astal/gtk4";
        description = "Astal GTK4 widget library";
        version = ../lib/astal/gtk4/version;
        dependencies = {inherit (dependency) "AstalIO-0.1" "Gtk-4.0";};
      }}
      ${genLib {
        flakepkg = "apps";
        gir = "Apps";
        description = "Application query library";
        version = ../lib/apps/version;
      }}
      ${genLib {
        flakepkg = "auth";
        gir = "Auth";
        authors = "kotontrion";
        description = "Authentication using pam";
        version = ../lib/auth/version;
      }}
      ${genLib {
        flakepkg = "battery";
        gir = "Battery";
        description = "DBus proxy for upowerd devices";
        version = ../lib/battery/version;
      }}
      ${genLib {
        flakepkg = "bluetooth";
        gir = "Bluetooth";
        description = "DBus proxy for bluez";
        version = ../lib/bluetooth/version;
      }}
      ${genLib {
        flakepkg = "cava";
        gir = "Cava";
        description = "Audio visualization library using cava";
        version = ../lib/cava/version;
        authors = "kotontrion";
      }}
      ${genLib {
        flakepkg = "greet";
        gir = "Greet";
        description = "IPC client for greetd";
        version = ../lib/greet/version;
      }}
      ${genLib {
        flakepkg = "hyprland";
        gir = "Hyprland";
        description = "IPC client for Hyprland";
        version = ../lib/hyprland/version;
      }}
      ${genLib {
        flakepkg = "mpris";
        gir = "Mpris";
        description = "Control mpris players";
        version = ../lib/mpris/version;
      }}
      ${genLib {
        flakepkg = "network";
        gir = "Network";
        description = "NetworkManager wrapper library";
        version = ../lib/network/version;
        dependencies = {inherit (dependency) "NM-1.0";}; # FIXME: why does this not work?
      }}
      ${genLib {
        flakepkg = "notifd";
        gir = "Notifd";
        description = "Notification daemon library";
        version = ../lib/notifd/version;
      }}
      ${genLib {
        flakepkg = "powerprofiles";
        gir = "PowerProfiles";
        description = "DBus proxy for upowerd profiles";
        version = ../lib/powerprofiles/version;
      }}
      ${genLib {
        flakepkg = "river";
        gir = "River";
        description = "IPC client for River";
        version = ../lib/river/version;
        authors = "kotontrion";
      }}
      ${genLib {
        flakepkg = "tray";
        gir = "Tray";
        description = "StatusNotifierItem implementation";
        version = ../lib/tray/version;
        authors = "kotontrion";
      }}
      ${genLib {
        flakepkg = "wireplumber";
        gir = "Wp";
        description = "Wrapper library over the wireplumber API";
        version = ../lib/wireplumber/version;
        authors = "kotontrion";
        dependencies = {inherit (dependency) "WP-0.5";}; # FIXME: why does this not work?
      }}
      runHook postInstall
    '';
  }
