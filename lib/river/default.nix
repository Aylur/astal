{
  mkAstalPkg,
  pkgs,
  self,
  ...
}: let
  wl-vapi-gen = pkgs.stdenv.mkDerivation {
    pname = "wl-vapi-gen";
    version = "v1.0.0";
    src = pkgs.fetchFromGitHub {
      owner = "kotontrion";
      repo = "wl-vapi-gen";
      rev = "1.0.0";
      hash = "sha256-XdgYmxW0ndH6szq7VJ+XQEnWKHCyaWoBwEQREZnTm98=";
    };

    nativeBuildInputs = with pkgs; [
      meson
      ninja
      python3
    ];

    patchPhase = ''
      patchShebangs wl-vapi-gen.py
    '';
  };
in
  mkAstalPkg {
    pname = "astal-river";
    src = ./.;
    packages = [
      self.packages.${pkgs.stdenv.hostPlatform.system}.wl
      wl-vapi-gen
    ];

    libname = "river";
    authors = "kotontrion";
    name = "AstalRiver";
    description = "IPC client for River";
    dependencies = ["AstalWl-0.1"];
  }
