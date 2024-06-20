version_split = meson.project_version().split('.')
api_version = version_split[0] + '.' + version_split[1]
gir = 'AstalNotifd-' + api_version + '.gir'
typelib = 'AstalNotifd-' + api_version + '.typelib'
so = 'libastal-notifd.so.' + meson.project_version()

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

sources = [
  config,
  'daemon.vala',
  'notifd.vala',
  'notification.vala',
  'proxy.vala',
]

if get_option('lib')
  lib = library(
    meson.project_name(),
    sources,
    dependencies: deps,
    vala_header: meson.project_name() + api_version + '.h',
    vala_vapi: meson.project_name() + api_version + '.vapi',
    vala_gir: gir,
    version: meson.project_version(),
    install: true,
    install_dir: [true, true, true, true],
  )

  import('pkgconfig').generate(
    description: 'libastal-notifd',
    libraries: lib,
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
