{
  mkAstalPkg,
  pkgs,
  ...
}: let
  libcava = pkgs.stdenv.mkDerivation rec {
    pname = "cava";
    version = "1.0.0";

    src = pkgs.fetchFromGitHub {
      owner = "LukashonakV";
      repo = "cava";
      rev = "${version}";
      hash = "sha256-0r5aAmTs+FcmS501tNYKxG9H+Pq6i32BDRBEjWW6M74=";
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
    name = "AstalCava";
    description = "Audio visualization library using cava";
  }
