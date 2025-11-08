project(
  'astal-wl',
  'vala',
  'c',
  version: run_command('cat', join_paths(meson.project_source_root(), 'version')).stdout().strip(),
  meson_version: '>= 0.63.0',
  default_options: [
    'warning_level=2',
    'werror=false',
    'c_std=gnu11',
  ],
)

version_split = meson.project_version().split('.')
api_version = version_split[0] + '.' + version_split[1]
namespace = 'AstalWl'
gir = namespace + '-' + api_version + '.gir'
typelib = namespace + '-' + api_version + '.typelib'

config = configure_file(
  input: 'config.vala.in',
  output: 'config.vala',
  configuration: {
    'API_VERSION': api_version,
    'VERSION': meson.project_version(),
    'MAJOR_VERSION': version_split[0],
    'MINOR_VERSION': version_split[1],
    'MICRO_VERSION': version_split[2],
  },
)

deps = [
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
  dependency('wayland-client'),
]

sources = [config] + files(
  'wl_display.c',
  'registry.vala',
  'wl-source.vala',
  'output.vala'
)

lib = library(
  meson.project_name(),
  sources,
  dependencies: deps,
  vala_header: meson.project_name() + '.h',
  vala_vapi: meson.project_name() + '-' + api_version + '.vapi',
  vala_args: ['--vapi-comments'],
  version: meson.project_version(),
  install: true,
  install_dir: [true, true, true],
)

pkgs = []
foreach dep : deps
  pkgs += ['--pkg=' + dep.name()]
endforeach

gir_tgt = custom_target(
  gir,
  command: [find_program('python3'), files('gir.py'), meson.project_name(), gir]
  + pkgs
  + sources,
  input: sources,
  depends: lib,
  output: gir,
  install: true,
  install_dir: get_option('datadir') / 'gir-1.0',
)

custom_target(
  typelib,
  command: [
    find_program('g-ir-compiler'),
    '--output', '@OUTPUT@',
    '--shared-library', get_option('prefix') / get_option('libdir') / '@PLAINNAME@',
    meson.current_build_dir() / gir,
  ],
  input: lib,
  output: typelib,
  depends: [lib, gir_tgt],
  install: true,
  install_dir: get_option('libdir') / 'girepository-1.0',
)

import('pkgconfig').generate(
  lib,
  name: meson.project_name(),
  filebase: meson.project_name() + '-' + api_version,
  version: meson.project_version(),
  subdirs: meson.project_name(),
  requires: deps,
  install_dir: get_option('libdir') / 'pkgconfig',
)
