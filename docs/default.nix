{
  self,
  pkgs,
}: let
  inherit (builtins) removeAttrs concatStringsSep map attrValues;
  packages = attrValues (removeAttrs self.packages.${pkgs.stdenv.hostPlatform.system} ["default" "docs"]);

  cp = pkg: ''
    doc="${pkg.doc}/share/doc"
    name=$(ls $doc)

    mkdir -p "$out/$name"
    cp -r "$doc/$name" $out
  '';
in
  pkgs.runCommand "docs" {} (concatStringsSep "" (map cp packages))
