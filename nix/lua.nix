defaults: {
  pkgs ? defaults.pkgs,
  astal ? defaults.astal,
  name ? "astal-lua",
  src,
  extraLuaPackages ? (ps: []),
  extraPacakges ? [],
}: let
  lua = pkgs.lua.withPackages (ps:
    (extraLuaPackages ps)
    ++ [
      ps.lgi
      (ps.luaPackages.toLuaModule (pkgs.stdenv.mkDerivation {
        name = "astal";
        version = "0.1.0";
        src = "${astal}/core/lua";
        dontBuild = true;
        installPhase = ''
          mkdir -p $out/share/lua/${ps.lua.luaversion}/astal
          cp -r astal/* $out/share/lua/${ps.lua.luaversion}/astal
        '';
      }))
    ]);

  nativeBuildInputs = with pkgs; [
    wrapGAppsHook
    gobject-introspection
  ];

  buildInputs =
    extraPacakges
    ++ [
      lua
      astal.packages.${pkgs.system}.default
    ];

  script = pkgs.writeScript "astal-lua" ''
    #!${lua}/bin/lua
    package.path = package.path .. ";${src}/?.lua"
    require "app"
  '';
in
  pkgs.stdenv.mkDerivation {
    inherit nativeBuildInputs buildInputs src name;

    installPhase = ''
      mkdir -p $out/bin
      cp -r * $out/bin
      cp ${script} $out/bin/${name}
      chmod +x $out/bin/${name}
    '';

    preFixup = ''
      gappsWrapperArgs+=(
        --prefix PATH : "${pkgs.lib.makeBinPath extraPacakges}"
      )
    '';
  }
