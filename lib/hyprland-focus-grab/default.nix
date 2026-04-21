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
in {
  gtk3 = mkAstalPkg {
    pname = "astal-hyprland-focus-grab-gtk3";
    src = ./.;
    packages = [
      self.packages.${pkgs.stdenv.hostPlatform.system}.wl
      wl-vapi-gen
      pkgs.gtk3
    ];

    mesonFlags = [
      "-Dgtk-version=3"
    ];

    libname = "hyprland-focus-grab-gtk3";
    authors = "Mabi19";
    gir-suffix = "HyprlandFocusGrab";
    api-ver = "3.0";
    description = "Allows windows to behave like popups on hyprland";
    dependencies = ["AstalWl-0.1"];
  };

  gtk4 = mkAstalPkg {
    pname = "astal-hyprland-focus-grab-gtk4";
    src = ./.;
    packages = [
      self.packages.${pkgs.stdenv.hostPlatform.system}.wl
      wl-vapi-gen
      pkgs.gtk4
    ];

    mesonFlags = [
      "-Dgtk-version=4"
    ];

    libname = "hyprland-focus-grab-gtk4";
    authors = "Mabi19";
    gir-suffix = "HyprlandFocusGrab";
    api-ver = "4.0";
    description = "Allows windows to behave like popups on hyprland";
    dependencies = ["AstalWl-0.1"];
  };
}
