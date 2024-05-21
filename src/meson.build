version_split = meson.project_version().split('.')
api_version = version_split[0] + '.' + version_split[1]
notifd_gir = 'AstalNotifd-' + api_version + '.gir'
notifd_typelib = 'AstalNotifd-' + api_version + '.typelib'
notifd_so = 'libastal-notifd.so.' + meson.project_version()

deps = [
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
  dependency('json-glib-1.0'),
  dependency('gdk-pixbuf-2.0'),
]

sources = files(
  'daemon.vala',
  'notifd.vala',
  'proxy.vala',
  'notification.vala',
)

libnotifd = library(
  meson.project_name(),
  sources,
  dependencies: deps,
  vala_header: meson.project_name() + '.h',
  vala_vapi: meson.project_name() + '.vapi',
  vala_gir: notifd_gir,
  version: meson.project_version(),
  install: true,
  install_dir: [true, true, true, true],
)

import('pkgconfig').generate(
  description: 'libastal-notifd',
  libraries: libnotifd,
  name: meson.project_name(),
  filebase: meson.project_name() + '-' + api_version,
  version: meson.project_version(),
  subdirs: meson.project_name(),
  requires: 'gio-2.0',
  install_dir: get_option('libdir') / 'pkgconfig',
)

if get_option('typelib')
  custom_target(
    notifd_typelib,
    command: [
      find_program('g-ir-compiler'),
      '--output', '@OUTPUT@',
      '--shared-library', get_option('prefix') / get_option('libdir') / '@PLAINNAME@',
      meson.current_build_dir() / notifd_gir,
    ],
    input: libnotifd,
    output: notifd_typelib,
    depends: libnotifd,
    install: true,
    install_dir: get_option('libdir') / 'girepository-1.0',
  )
endif

if get_option('cli_client')
  cli = configure_file(
    input: 'cli.vala.in',
    output: 'cli.vala',
    configuration: {
      'VERSION': meson.project_version(),
    },
  )
  executable(
    meson.project_name(),
    [cli, sources],
    dependencies: deps,
    install: true,
  )
endif
