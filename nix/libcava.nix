{
  stdenv,
  fetchFromGitHub,
  autoreconfHook,
  autoconf-archive,
  alsa-lib,
  fftw,
  iniparser,
  libpulseaudio,
  portaudio,
  sndio,
  SDL2,
  libGL,
  pipewire,
  jack2,
  ncurses,
  pkgconf,
  meson,
  ninja,
}:
stdenv.mkDerivation rec {
  pname = "cava";
  version = "0.10.3";

  src = fetchFromGitHub {
    owner = "LukashonakV";
    repo = "cava";
    rev = "0.10.3";
    hash = "sha256-ZDFbI69ECsUTjbhlw2kHRufZbQMu+FQSMmncCJ5pagg=";
  };

  buildInputs = [
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

  propagatedBuildInputs = [
    fftw
  ];

  nativeBuildInputs = [
    autoreconfHook
    autoconf-archive
    pkgconf
    meson
    ninja
  ];

  preAutoreconf = ''
    echo ${version} > version
  '';
}
