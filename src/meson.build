version_split = meson.project_version().split('.')
api_version = version_split[0] + '.' + version_split[1]
astal_gir = 'Astal-' + api_version + '.gir'
astal_typelib = 'Astal-' + api_version + '.typelib'
astal_so = 'libastal.so.' + meson.project_version()

deps = [
  dependency('glib-2.0'),
  dependency('gio-unix-2.0'),
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
  dependency('gtk+-3.0'),
  dependency('gdk-pixbuf-2.0'),
  dependency('gtk-layer-shell-0'),
]

sources = files(
  'widget/box.vala',
  'widget/button.vala',
  'widget/centerbox.vala',
  'widget/eventbox.vala',
  'widget/icon.vala',
  # 'widget/circularprogress.vala', # TODO: math lib -X -lm
  'widget/widget.vala',
  'widget/window.vala',
  'astal.vala',
  'process.vala',
  'time.vala',
  'variable.vala',
)

libastal = library(
  meson.project_name(),
  sources,
  dependencies: deps,
  vala_header: meson.project_name() + '.h',
  vala_vapi: meson.project_name() + '.vapi',
  vala_gir: astal_gir,
  version: meson.project_version(),
  install: true,
  install_dir: [true, true, true, true],
)

import('pkgconfig').generate(
  description: 'libastal',
  libraries: libastal,
  name: meson.project_name(),
  filebase: meson.project_name() + '-' + api_version,
  version: meson.project_version(),
  subdirs: meson.project_name(),
  requires: 'gio-2.0',
  install_dir: get_option('libdir') / 'pkgconfig',
)

if get_option('typelib')
  custom_target(
    astal_typelib,
    command: [
      find_program('g-ir-compiler'),
      '--output', '@OUTPUT@',
      '--shared-library', get_option('prefix') / get_option('libdir') / '@PLAINNAME@',
      meson.current_build_dir() / astal_gir,
    ],
    input: libastal,
    output: astal_typelib,
    depends: libastal,
    install: true,
    install_dir: get_option('libdir') / 'girepository-1.0',
  )
endif

if get_option('cli_client')
  executable(
    meson.project_name(),
    configure_file(
      input: 'client.vala.in',
      output: 'client.vala',
      configuration: {
        'VERSION': meson.project_version(),
      },
    ),
    dependencies: deps,
    install: true,
  )
endif
