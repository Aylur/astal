{
  self,
  pkgs,
}: let
  lua = pkgs.lua.withPackages (ps: [ps.lgi]);

  python = pkgs.python3.withPackages (ps: [
    ps.pygobject3
    ps.pygobject-stubs
  ]);

  buildInputs = with pkgs; [
    wrapGAppsHook4
    gobject-introspection
    meson
    pkg-config
    ninja
    vala
    gtk3
    gtk4
    gtk-layer-shell
    gtk4-layer-shell
    json-glib
    pam
    gvfs
    networkmanager
    gdk-pixbuf
    wireplumber
    libdbusmenu-gtk3
    wayland
    blueprint-compiler
    libadwaita
    wayland-scanner
    dart-sass
    esbuild
    lua
    python
    gjs
  ];

  gsettings =
    pkgs.writers.writeNu "gsettings"
    # nu
    ''
      let schema_paths = (
        $env.GSETTINGS_SCHEMAS_PATH?
        | default ""
        | split row (char esep)
        | each { |p| $p | path join "glib-2.0" "schemas" }
      )

      let schema_dirs = (
        $env.GSETTINGS_SCHEMA_DIR?
        | default ""
        | split row (char esep)
      )

      print (
        [...$schema_paths ...$schema_dirs]
        | str join (char esep)
      )
    '';
in {
  default = pkgs.mkShell {
    packages = buildInputs;
  };
  astal = pkgs.mkShell {
    packages =
      buildInputs
      ++ builtins.attrValues (
        builtins.removeAttrs self.packages.${pkgs.stdenv.hostPlatform.system} ["docs"]
      );

    shellHook = ''
      export GSETTINGS_SCHEMA_DIR="$(${gsettings})"
    '';
  };
}
