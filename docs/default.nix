{
  self,
  pkgs,
}: let
  inherit (builtins) removeAttrs concatStringsSep map attrValues;
  packages = attrValues (removeAttrs self.packages.${pkgs.system} ["default" "docs" "gjs"]);

  cp = pkg: ''
    doc="${pkg.doc}/share/doc"
    name=$(ls $doc)

    mkdir -p "$out/$name"
    cp -r "$doc/$name" $out
  '';
in
  pkgs.runCommand "docs" {} (concatStringsSep "" (map cp packages))
