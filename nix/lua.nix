defaults: {
  pkgs ? defaults.pkgs,
  astal ? defaults.astal,
  name ? "astal-lua",
  src,
  extraLuaPackages ? (ps: []),
  extraPackages ? [],
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

  script = ''
    #!${lua}/bin/lua
    package.path = package.path .. ";${src}/?.lua"
    require "app"
  '';
in
  pkgs.stdenvNoCC.mkDerivation {
    inherit src name;

    nativeBuildInputs = with pkgs; [
      wrapGAppsHook
      gobject-introspection
    ];

    buildInputs =
      extraPackages
      ++ [
        lua
        astal.packages.${pkgs.system}.default
      ];

    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin
      cp -r * $out/bin
      echo '${script}' > astal-lua
      install -m 755 astal-lua $out/bin/${name}
      runHook postInstall
    '';

    gappsWrapperArgs = [
      "--prefix PATH : ${pkgs.lib.makeBinPath extraPackages}"
    ];
  }
