project(
  'astal-io',
  'vala',
  'c',
  version: run_command('cat', join_paths(meson.project_source_root(), 'version')).stdout().strip(),
  meson_version: '>= 0.62.0',
  default_options: [
    'warning_level=2',
    'werror=false',
    'c_std=gnu11',
  ],
)

version_split = meson.project_version().split('.')
api_version = version_split[0] + '.' + version_split[1]
gir = 'AstalIO-' + api_version + '.gir'
typelib = 'AstalIO-' + api_version + '.typelib'
libdir = get_option('prefix') / get_option('libdir')
pkgdatadir = get_option('prefix') / get_option('datadir') / 'astal'

config = configure_file(
  input: 'config.vala.in',
  output: 'config.vala',
  configuration: {
    'VERSION': meson.project_version(),
    'MAJOR_VERSION': version_split[0],
    'MINOR_VERSION': version_split[1],
    'MICRO_VERSION': version_split[2],
  },
)

deps = [
  dependency('glib-2.0'),
  dependency('gio-unix-2.0'),
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
]

sources = [
  config,
  'application.vala',
  'file.vala',
  'process.vala',
  'time.vala',
  'variable.vala',
]

lib = library(
  meson.project_name(),
  sources,
  dependencies: deps,
  vala_header: meson.project_name() + '.h',
  vala_vapi: meson.project_name() + '-' + api_version + '.vapi',
  vala_gir: gir,
  version: meson.project_version(),
  install: true,
  install_dir: [true, true, true, true],
)

import('pkgconfig').generate(
  lib,
  name: meson.project_name(),
  filebase: meson.project_name() + '-' + api_version,
  version: meson.project_version(),
  subdirs: meson.project_name(),
  requires: deps,
  install_dir: libdir / 'pkgconfig',
)

custom_target(
  typelib,
  command: [
    find_program('g-ir-compiler'),
    '--output', '@OUTPUT@',
    '--shared-library', libdir / '@PLAINNAME@',
    meson.current_build_dir() / gir,
  ],
  input: lib,
  output: typelib,
  depends: lib,
  install: true,
  install_dir: libdir / 'girepository-1.0',
)

executable(
  'astal',
  ['cli.vala', sources],
  dependencies: deps,
  install: true,
)
