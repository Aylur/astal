{
  flakePkgs,
  stdenvNoCC,
  formats,
  gi-docgen,
  glib,
  json-glib,
  gobject-introspection,
  gtk3,
  gtk-layer-shell,
  gdk-pixbuf,
  libdbusmenu-gtk3,
  wireplumber,
  networkmanager,
  ...
}: let
  toTOML = (formats.toml {}).generate;

  genRefForPkg = {
    name,
    pkg,
    outPath,
    metaData,
  }: let
    tomlMetaData = toTOML name metaData;
    devOutput = flakePkgs.${pkg}.dev;
  in ''
    mkdir -p $out/${outPath}

    gi-docgen generate \
      -C ${tomlMetaData} ${devOutput}/share/gir-1.0/${name}-0.1.gir

    # Copy files
    cp -rv ${name}-0.1/* $out/${outPath}
  '';

  dependency = {
    "GObject-2.0" = {
      name = "GObject";
      description = "The base type system library";
      docs_url = "https://developer.gnome.org/gobject/stable";
    };
  };

  lib = name: namespace: description: {
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
          browse_url = "https://github.com/Aylur/Astal";
          repository_url = "https://github.com/Aylur/Aylur.git";
          website_url = "https://aylur.github.io/astal";
        };

        dependencies = {
          inherit (dependency) "GObject-2.0";
          inherit dependencies;
        };
      };
    };
in
  stdenvNoCC.mkDerivation {
    name = "library-reference";
    src = builtins.path {
      name = "library-reference-src";
      path = ./.;
    };

    nativeBuildInputs = [
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
      runHook postInstall
    '';
  }
