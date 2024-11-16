project(
  'astal-tray',
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

assert(
  get_option('lib') or get_option('cli'),
  'Either lib or cli option must be set to true.',
)

version_split = meson.project_version().split('.')
api_version = version_split[0] + '.' + version_split[1]
namespace = 'AstalTray'
gir = namespace + '-' + api_version + '.gir'
typelib = namespace + '-' + api_version + '.typelib'

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
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
  dependency('json-glib-1.0'),
  dependency('gdk-pixbuf-2.0'),
  dependency('gtk+-3.0'),
]

dbusmenu_cflags = run_command(
  find_program('pkg-config', required: true),
  '--cflags', 'dbusmenu-gtk3-0.4',
  'gobject-introspection-1.0',
  'gobject-2.0',
  'glib-2.0',
  capture: true,
  check: true,
).stdout().strip()

dbusmenu_libs = run_command(
  find_program('pkg-config', required: true),
  '--libs', 'dbusmenu-gtk3-0.4',
  'gobject-introspection-1.0',
  'gobject-2.0',
  'glib-2.0',
  capture: true,
  check: true,
).stdout().strip()

sources = [config] + files('tray.vala', 'watcher.vala', 'trayItem.vala')

if get_option('lib')
  lib = library(
    meson.project_name(),
    sources,
    dependencies: deps,
    vala_header: meson.project_name() + '.h',
    vala_vapi: meson.project_name() + '-' + api_version + '.vapi',
    vala_args: ['--vapi-comments', '--pkg', 'DbusmenuGtk3-0.4', '--pkg', 'Dbusmenu-0.4'],
    version: meson.project_version(),
    c_args: dbusmenu_cflags.split(' '),
    link_args: dbusmenu_libs.split(' '),
    install: true,
    install_dir: [true, true, true]
  )

  pkgs = ['--pkg', 'DbusmenuGtk3-0.4', '--pkg', 'Dbusmenu-0.4']
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
  #
  # custom_target(
  #   typelib,
  #   command: [
  #     find_program('g-ir-compiler'),
  #     '--output', '@OUTPUT@',
  #     '--shared-library', get_option('prefix') / get_option('libdir') / '@PLAINNAME@',
  #     meson.current_build_dir() / gir,
  #   ],
  #   input: lib,
  #   output: typelib,
  #   depends: lib,
  #   install: true,
  #   install_dir: get_option('libdir') / 'girepository-1.0',
  # )
endif

if get_option('cli')
  executable(
    meson.project_name(),
    ['cli.vala', sources],
    dependencies: deps,
    vala_args: ['--pkg', 'DbusmenuGtk3-0.4', '--pkg', 'Dbusmenu-0.4'],
    c_args: dbusmenu_cflags.split(' '),
    link_args: dbusmenu_libs.split(' '),
    install: true,
  )
endif
