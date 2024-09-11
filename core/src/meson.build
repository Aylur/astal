version_split = meson.project_version().split('.')
api_version = version_split[0] + '.' + version_split[1]
gir = 'Astal-' + api_version + '.gir'
typelib = 'Astal-' + api_version + '.typelib'

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

pkgconfig_deps = [
  dependency('glib-2.0'),
  dependency('gio-unix-2.0'),
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
  dependency('gtk+-3.0'),
  dependency('gdk-pixbuf-2.0'),
  dependency('gtk-layer-shell-0'),
]

deps = pkgconfig_deps + meson.get_compiler('c').find_library('m')

sources = [
  config,
  'widget/box.vala',
  'widget/button.vala',
  'widget/centerbox.vala',
  'widget/circularprogress.vala',
  'widget/eventbox.vala',
  'widget/icon.vala',
  'widget/label.vala',
  'widget/levelbar.vala',
  'widget/overlay.vala',
  'widget/scrollable.vala',
  'widget/slider.vala',
  'widget/stack.vala',
  'widget/widget.vala',
  'widget/window.vala',
  'astal.vala',
  'file.vala',
  'process.vala',
  'time.vala',
  'variable.vala',
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
    requires: pkgconfig_deps,
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
endif

if get_option('cli')
  executable(
    meson.project_name(),
    ['cli.vala', sources],
    dependencies: deps,
    install: true,
  )
endif
