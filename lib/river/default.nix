{
  mkAstalPkg,
  pkgs,
  self,
  ...
}: let
   wl-vapi-gen = pkgs.stdenv.mkDerivation {
    pname = "wl-vapi-gen";
    version = "v0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "kotontrion";
      repo = "wl-vapi-gen";
      rev = "b4a7eda9404edbf192faf474be1372f77f584dfd";
      hash = "sha256-uIl1mxoQRmHNRLbFLDuymK//H0Kse7KpiHf8CQ8fnI0=";
    };
 
    nativeBuildInputs = with pkgs; [
      meson
      ninja
      python3
    ];
  };
in
  mkAstalPkg {
    pname = "astal-river";
    src = ./.;
    packages = [
      self.packages.${pkgs.system}.wl
      wl-vapi-gen
    ];

    libname = "river";
    authors = "kotontrion";
    gir-suffix = "River";
    description = "IPC client for River";
  }
