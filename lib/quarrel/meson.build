project(
  'quarrel',
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

version = meson.project_version()
name = meson.project_name()
prefix = get_option('prefix')
datadir = prefix / get_option('datadir')
libdir = prefix / get_option('libdir')
api_version = '0.1'
gir_name = 'Quarrel-0.1.gir'
typelib_name = 'Quarrel-0.1.typelib'

deps = [
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
]

sources = files(
  'src/command.vala',
  'src/help.vala',
  'src/opt.vala',
)

lib = library(
  name,
  sources,
  dependencies: deps,
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
    '--property', 'Command.parent',
    '--property', 'Command.about-text',
    '--property', 'Command.name',
    '--property', 'Command.args',
    '--property', 'Opt.description',
    '--property', 'Opt.name',
    '--property', 'Opt.long',
    '--property', 'Opt.short',
    '--property', 'FileArrayOpt.value',
    '--property', 'FileOpt.value',
    '--property', 'StringArrayOpt.value',
    '--property', 'StringOpt.value',
    sources,
  ],
  input: sources,
  depends: lib,
  output: gir_name,
  install: true,
  install_dir: datadir / 'gir-1.0',
)

custom_target(
  typelib_name,
  command: [
    find_program('g-ir-compiler'),
    '--output', '@OUTPUT@',
    '--shared-library', libdir / '@PLAINNAME@',
    meson.current_build_dir() / gir_name,
  ],
  input: lib,
  output: typelib_name,
  depends: [lib, gir],
  install: true,
  install_dir: libdir / 'girepository-1.0',
)

import('pkgconfig').generate(
  lib,
  name: name,
  filebase: name + '-' + api_version,
  version: version,
  subdirs: name,
  requires: deps,
  install_dir: libdir / 'pkgconfig',
)
