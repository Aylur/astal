project(
  'astal-hyprland-focus-grab',
  'vala',
  'c',
  meson_version: '>= 1.5.0',
  version: run_command('cat', join_paths(meson.project_source_root(), 'version')).stdout().strip(),
  default_options: ['warning_level=2', 'werror=false', 'c_std=gnu11'],
)

version = meson.project_version()
name = meson.project_name()
version_split = version.split('.')
api_version = '0.1'
gir_name = 'AstalHyprlandFocusGrab-0.1.gir'
typelib_name = 'AstalHyprlandFocusGrab-0.1.typelib'

config = configure_file(
  input: 'src/config.vala.in',
  output: 'config.vala',
  configuration: {
    'API_VERSION': api_version,
    'VERSION': version,
    'MAJOR_VERSION': version_split[0],
    'MINOR_VERSION': version_split[1],
    'MICRO_VERSION': version_split[2],
  },
)

subdir('protocols')

deps = [
  dependency('gtk4'),
  dependency('gtk4-wayland'),
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('astal-wl-0.1'),
  dependency('wayland-client'),
]

sources = [config] + files('src/context.vala', 'src/grab.vala', 'src/manager.vala')

lib = library(
  name,
  sources,
  dependencies: deps + protocol_deps,
  vala_args: ['--vapi-comments'],
  vala_header: name + '.h',
  vala_vapi: name + '-' + api_version + '.vapi',
  version: version,
  install: true,
  install_dir: [true, true, true],
)

gir = custom_target(
  gir_name,
  command: [
    find_program('python3'),
    files('gir.py'),
    '--name', name,
    '--gir', gir_name,
    '--pkgs', ':'.join(
      'wayland-client',
      'astal-wl-0.1',
      'gtk4',
      'gtk4-wayland',
    ),
    '@INPUT@',
  ],
  input: sources + protocol_vapis,
  depends: lib,
  output: gir_name,
  install: true,
  install_dir: get_option('datadir') / 'gir-1.0',
)

custom_target(
  typelib_name,
  command: [
    find_program('g-ir-compiler'),
    '--output', '@OUTPUT@',
    '--shared-library', get_option('prefix') / get_option('libdir') / '@PLAINNAME@',
    meson.current_build_dir() / gir_name,
  ],
  input: lib,
  output: typelib_name,
  depends: [lib, gir],
  install: true,
  install_dir: get_option('libdir') / 'girepository-1.0',
)

import('pkgconfig').generate(
  lib,
  name: name,
  filebase: name + '-' + api_version,
  version: version,
  subdirs: name,
  requires: deps,
  install_dir: get_option('libdir') / 'pkgconfig',
)
