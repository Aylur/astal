{
  self,
  pkgs,
}: let
  inherit (builtins) removeAttrs concatStringsSep map attrValues;
  packages = attrValues (removeAttrs self.packages.${pkgs.stdenv.hostPlatform.system} ["default" "docs"]);

  href = "https://astal.dev/guide/libraries/references";

  index =
    /*
    html
    */
    ''
      <!doctype html>
      <html lang="en">
        <head>
          <meta charset="utf-8" />
          <title>Redirecting…</title>
          <meta http-equiv="refresh" content="0; url=${href}" />
          <link rel="canonical" href="${href}" />
        </head>
        <body>
          <p>Redirecting to <a href="${href}">astal.dev</a></p>
          <script>location.replace("${href}")</script>
        </body>
      </html>
    '';

  map_packages = fn: concatStringsSep "" (map fn packages);
in
  # pkgs.runCommand "docs" {} (concatStringsSep "" (map cp packages))
  pkgs.runCommand "docs" {inherit index;} ''
    ${map_packages (pkg: ''
      doc="${pkg.doc}/share/doc"
      name=$(ls $doc)

      mkdir -p "$out/$name"
      cp -r "$doc/$name" $out
    '')}

    echo "$index" > $out/index.html
  ''
