pkgs: let
  inherit (builtins) elem elemAt readFile replaceStrings splitVersion toJSON;
  inherit (pkgs.lib) filterAttrs;

  readVer = file: replaceStrings ["\n"] [""] (readFile file);

  toTOML = (pkgs.formats.toml {}).generate;

  docgen = pkgs.gi-docgen.overrideAttrs {
    patches = [./gi-docgen.patch];
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
    baseURLs = ${toJSON [
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
  {
    src,
    pname,
    libname,
    gir-suffix,
    authors,
    description,
    dependencies ? [],
    repo-path ? libname,
    website-path ? libname,
    nativeBuildInputs ? [],
    packages ? [],
    postUnpack ? "",
  }: let
    version = readVer "${src}/version";

    ver = splitVersion version;
    api-ver = "${elemAt ver 0}.${elemAt ver 1}";
    girName = "Astal${gir-suffix}-${api-ver}";
  in
    pkgs.stdenv.mkDerivation {
      inherit pname src version;
      outputs = ["out" "dev" "doc"];

      nativeBuildInputs = with pkgs;
        [
          wrapGAppsHook3
          gobject-introspection
          meson
          pkg-config
          ninja
          vala
          wayland
          wayland-scanner
          python3
        ]
        ++ nativeBuildInputs;

      propagatedBuildInputs = with pkgs;
        [
          glib
        ]
        ++ packages;

      postUnpack = ''
        cp --remove-destination ${../lib/gir.py} $sourceRoot/gir.py
        ${postUnpack}
      '';

      postInstall = let
        data = toTOML libname {
          library = {
            inherit description authors version;
            license = "LGPL-2.1";
            browse_url = "https://github.com/Aylur/astal/tree/main/lib/${repo-path}";
            repository_url = "https://github.com/aylur/aylur.git";
            website_url = "https://aylur.github.io/astal/guide/libraries/${website-path}";
            dependencies = ["GObject-2.0"] ++ dependencies;
          };

          extra.urlmap_file = "urlmap.js";
          dependencies =
            {inherit (dependency) "GObject-2.0";}
            // (filterAttrs (n: _: elem n dependencies) dependency);
        };
      in ''
        gir="${girName}.gir"

        mkdir -p $out/share/doc/${website-path}
        cat ${urlmap} > urlmap.js

        if [ -d "src" ]; then
          gir="src/$gir"
        fi

        ${docgen}/bin/gi-docgen generate --config ${data} $gir
        mv ${girName}/* $out/share/doc/${website-path}
      '';

      passthru = {
        inherit girName;
      };

      meta = {
        inherit description;
        homepage = "https://aylur.github.io/astal";
        license = pkgs.lib.licenses.lgpl21;
      };
    }
