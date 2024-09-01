{
  pkgs,
  astal,
}: let
  toTOML = (pkgs.formats.toml {}).generate;

  gen = pkg: name: out: toml: ''
    mkdir -p $out/${out}
    gi-docgen generate -C ${toTOML name toml} ${astal.${pkg}.dev}/share/gir-1.0/${name}-0.1.gir
    cp -r ${name}-0.1/* $out/${out}
  '';

  lib = name: namespace: description: {
    authors ? "Aylur",
    dependencies ? {},
    out ? "libastal/${name}",
  }:
    gen name "Astal${namespace}" out {
      library = {
        inherit description authors;
        license = "LGPL-2.1";
        browse_url = "https://github.com/Aylur/Astal";
        repository_url = "https://github.com/Aylur/Aylur.git";
        website_url = "https://aylur.github.io/astal";
      };
      dependencies = {
        inherit (dependency) "GObject-2.0";
        inherit dependencies;
      };
    };

  dependency = {
    "GObject-2.0" = {
      name = "GObject";
      description = "The base type system library";
      docs_url = "https://developer.gnome.org/gobject/stable";
    };
  };
in
  pkgs.stdenv.mkDerivation {
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
    name = "library-reference";
    src = ./.;

    installPhase = ''
      ${lib "astal" "" "Astal core library" {out = "libastal";}}
      ${lib "apps" "Apps" "Application query library" {}}
      ${lib "auth" "Auth" "Authentication using pam" {authors = "kotontrion";}}
      ${lib "battery" "Battery" "DBus proxy for upowerd devices" {}}
      ${lib "bluetooth" "Bluetooth" "DBus proxy for bluez" {}}
      ${lib "hyprland" "Hyprland" "IPC client for Hyprland" {}}
      ${lib "mpris" "Mpris" "Control mpris players" {}}
      ${lib "network" "Network" "NetworkManager wrapper library" {}}
      ${lib "notifd" "Notifd" "Notification daemon library" {}}
      ${lib "powerprofiles" "PowerProfiles" "DBus proxy for upowerd profiles" {}}
      ${lib "river" "River" "IPC client for River" {authors = "kotontrion";}}
      ${lib "tray" "Tray" "StatusNotifierItem implementation" {authors = "kotontrion";}}
      ${lib "wireplumber" "Wp" "Wrapper library over the wireplumber API" {authors = "kotontrion";}}
    '';
  }
