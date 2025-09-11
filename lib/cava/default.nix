{
  mkAstalPkg,
  pkgs,
  ...
}: let
  libcava = pkgs.stdenv.mkDerivation rec {
    pname = "cava";
    version = "0.10.4";

    src = pkgs.fetchFromGitHub {
      owner = "LukashonakV";
      repo = "cava";
      rev = "0.10.4";
      hash = "sha256-9eTDqM+O1tA/3bEfd1apm8LbEcR9CVgELTIspSVPMKM=";
    };

    buildInputs = with pkgs; [
      alsa-lib
      libpulseaudio
      ncurses
      iniparser
      sndio
      SDL2
      libGL
      portaudio
      jack2
      pipewire
    ];

    propagatedBuildInputs = with pkgs; [
      fftw
    ];

    nativeBuildInputs = with pkgs; [
      autoreconfHook
      autoconf-archive
      pkgconf
      meson
      ninja
    ];

    preAutoreconf = ''
      echo ${version} > version
    '';
  };
in
  mkAstalPkg {
    pname = "astal-cava";
    src = ./.;
    packages = [libcava];

    libname = "cava";
    authors = "kotontrion";
    gir-suffix = "Cava";
    description = "Audio visualization library using cava";
  }
