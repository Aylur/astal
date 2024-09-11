{
  self,
  pkgs,
}: let
  lua = pkgs.lua.withPackages (ps: [
    ps.lgi
    (ps.luaPackages.toLuaModule (pkgs.stdenv.mkDerivation {
      name = "astal";
      src = "${self}/core/lua";
      dontBuild = true;
      installPhase = ''
        mkdir -p $out/share/lua/${ps.lua.luaversion}/astal
        cp -r astal/* $out/share/lua/${ps.lua.luaversion}/astal
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
    gtk-layer-shell
    json-glib
    pam
    gvfs
    networkmanager
    gdk-pixbuf
    wireplumber
    libdbusmenu-gtk3
    wayland

    lua
    python
    gjs
  ];
in {
  default = pkgs.mkShell {
    inherit buildInputs;
  };
  astal = pkgs.mkShell {
    buildInputs = buildInputs ++ (builtins.attrValues self.packages.${pkgs.system});
  };
}
