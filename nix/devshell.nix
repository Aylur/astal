{
  self,
  pkgs,
}: let
  lua = pkgs.lua.withPackages (ps: [
    ps.lgi
    (ps.luaPackages.toLuaModule (pkgs.stdenv.mkDerivation {
      name = "astal";
      src = "${self}/lang/lua/astal";
      dontBuild = true;
      installPhase = ''
        mkdir -p $out/share/lua/${ps.lua.luaversion}/astal
        cp -r * $out/share/lua/${ps.lua.luaversion}/astal
      '';
    }))
  ]);

  python = pkgs.python3.withPackages (ps: [
    ps.pygobject3
    ps.pygobject-stubs
  ]);

  buildInputs = with pkgs; [
    wrapGAppsHook
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

    dart-sass
    lua
    python
    gjs
  ];

  lsp = with pkgs; [
    nodejs
    mesonlsp
    vala-language-server
    vtsls
    vscode-langservers-extracted
    markdownlint-cli2
  ];
in {
  default = pkgs.mkShell {
    packages = buildInputs ++ lsp;
  };
  astal = pkgs.mkShell {
    packages =
      buildInputs
      ++ lsp
      ++ builtins.attrValues (
        builtins.removeAttrs self.packages.${pkgs.system} ["docs"]
      );
  };
}
