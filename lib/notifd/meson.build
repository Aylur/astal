project(
  'astal-notifd',
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

assert(
  get_option('lib') or get_option('cli'),
  'Either lib or cli option must be set to true.',
)

version_split = meson.project_version().split('.')
api_version = version_split[0] + '.' + version_split[1]
namespace = 'AstalNotifd'
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
]

sources = [config] + files(
  'daemon.vala',
  'notifd.vala',
  'notification.vala',
  'proxy.vala',
)

if get_option('lib')
  lib = library(
    meson.project_name(),
    sources,
    dependencies: deps,
    vala_args: ['--vapi-comments', '--ccode'],
    vala_header: meson.project_name() + '.h',
    vala_vapi: meson.project_name() + '-' + api_version + '.vapi',
    vala_gir: gir,
    version: meson.project_version(),
    install: true,
    install_dir: [true, true, true, true],
  )

  # import('gnome').generate_gir(
  #   lib,
  #   sources: [],
  #   nsversion: api_version,
  #   namespace: namespace,
  #   symbol_prefix: meson.project_name().replace('-', '_'),
  #   identifier_prefix: namespace,
  #   includes: ['GObject-2.0'],
  #   header: meson.project_name() + '.h',
  #   export_packages: meson.project_name() + '-' + api_version,
  #   install: true,
  # )

  # custom_target(
  #   gir,
  #   command: [
  #     find_program('g-ir-scanner'),
  #     '--namespace=' + namespace,
  #     '--nsversion=' + api_version,
  #     '--library=' + meson.project_name(),
  #     '--include=GObject-2.0',
  #     '--output=' + gir,
  #     '--symbol-prefix=' + meson.project_name().replace('-', '_'),
  #     '--identifier-prefix=' + namespace,
  #   ]
  #   + pkgs
  #   + ['@INPUT@'],
  #   output: gir,
  #   depends: lib,
  #   input: meson.current_build_dir() / meson.project_name() + '.h',
  #   install: true,
  #   install_dir: get_option('datadir') / 'gir-1.0',
  # )

  # custom_target(
  #   gir,
  #   command: [
  #     find_program('g-ir-scanner'),
  #     '--namespace=' + namespace,
  #     '--nsversion=' + api_version,
  #     '--library=' + meson.project_name(),
  #     '--include=GObject-2.0',
  #     '--output=' + gir,
  #     '--symbol-prefix=' + meson.project_name().replace('-', '_'),
  #     '--identifier-prefix=' + namespace,
  #   ]
  #   + pkgs
  #   + ['@INPUT@'],
  #   input: lib.extract_all_objects(),
  #   output: gir,
  #   depends: lib,
  #   install: true,
  #   install_dir: get_option('datadir') / 'gir-1.0',
  # )

  # pkgs = []
  # foreach dep : deps
  #   pkgs += ['--pkg=' + dep.name()]
  # endforeach
  #
  # gir_tgt = custom_target(
  #   gir,
  #   command: [find_program('valadoc'), '-o', 'docs', '--gir', gir] + pkgs + sources,
  #   input: sources,
  #   depends: lib,
  #   output: gir,
  #   install: true,
  #   install_dir: get_option('datadir') / 'gir-1.0',
  # )

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
    depends: lib,
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
endif

if get_option('cli')
  executable(
    meson.project_name(),
    ['cli.vala', sources],
    dependencies: deps,
    install: true,
  )
endif
