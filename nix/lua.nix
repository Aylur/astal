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
      (ps.luaPackages.toLuaModule (pkgs.stdenvNoCC.mkDerivation {
        name = "astal";
        src = "${astal}/lang/lua";
        dontBuild = true;
        installPhase = ''
          mkdir -p $out/share/lua/${ps.lua.luaversion}/astal
          cp -r * $out/share/lua/${ps.lua.luaversion}/astal
        '';
      }))
      (ps.luaPackages.toLuaModule (pkgs.stdenvNoCC.mkDerivation {
        inherit src name;
        dontBuild = true;
        installPhase = ''
          mkdir -p $out/share/lua/${ps.lua.luaversion}
          cp -r * $out/share/lua/${ps.lua.luaversion}
        '';
      }))
    ]);

  script = ''
    #!${lua}/bin/lua
    require "init"
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
        astal.packages.${pkgs.system}.io
        astal.packages.${pkgs.system}.astal3
      ];

    installPhase = ''
      runHook preInstall

      mkdir -p $out/bin
      cp -r * $out/bin
      echo '${script}' > astal-lua
      install -m 755 astal-lua $out/bin/${name}

      runHook postInstall
    '';

    preFixup = ''
      gappsWrapperArgs+=(
        --prefix PATH : "${pkgs.lib.makeBinPath extraPackages}"
      )
    '';
  }
