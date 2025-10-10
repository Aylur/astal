project(
  'astal-niri',
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
gir = 'AstalNiri-' + api_version + '.gir'
typelib = 'AstalNiri-' + api_version + '.typelib'

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
  dependency('gio-unix-2.0'),
  dependency('json-glib-1.0'),
]

sources = [
  config,
  'cli.vala',
  'ipc.vala',
  'niri.vala',
  'window.vala',
  'workspace.vala',
  'output.vala',
  'structs.vala',
  'overview.vala',
  'msg.vala',
]

if get_option('lib')
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
    install_dir: get_option('libdir') / 'pkgconfig',
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
    depends: lib,
    install: true,
    install_dir: get_option('libdir') / 'girepository-1.0',
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
