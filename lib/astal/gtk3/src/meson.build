version_split = meson.project_version().split('.')
api_version = version_split[0] + '.' + version_split[1]
gir = 'Astal-' + api_version + '.gir'
typelib = 'Astal-' + api_version + '.typelib'

vapi_dir = meson.current_source_dir() / 'vapi'
add_project_arguments(['--vapidir', vapi_dir], language: 'vala')

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
  dependency('astal-io-0.1'),
  dependency('glib-2.0'),
  dependency('gio-unix-2.0'),
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
  dependency('gtk+-3.0'),
  dependency('gdk-pixbuf-2.0'),
  dependency('gtk-layer-shell-0'),
  dependency('wayland-client'),
]

deps = pkgconfig_deps + meson.get_compiler('c').find_library('m')

wayland_protos = dependency('wayland-protocols')
wayland_scanner = find_program('wayland-scanner')

wl_protocol_dir = wayland_protos.get_variable(pkgconfig: 'pkgdatadir')

gen_client_header = generator(
  wayland_scanner,
  output: ['@BASENAME@-client.h'],
  arguments: ['-c', 'client-header', '@INPUT@', '@BUILD_DIR@/@BASENAME@-client.h'],
)

gen_private_code = generator(
  wayland_scanner,
  output: ['@BASENAME@.c'],
  arguments: ['-c', 'private-code', '@INPUT@', '@BUILD_DIR@/@BASENAME@.c'],
)

protocols = [
  join_paths(wl_protocol_dir, 'unstable/idle-inhibit/idle-inhibit-unstable-v1.xml'),
]

client_protocol_srcs = []

foreach protocol : protocols
  client_header = gen_client_header.process(protocol)
  code = gen_private_code.process(protocol)
  client_protocol_srcs += [client_header, code]
endforeach

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
  'application.vala',
  'idle-inhibit.h',
  'idle-inhibit.c',
] + client_protocol_srcs

lib = library(
  meson.project_name(),
  sources,
  dependencies: deps,
  vala_args: ['--pkg', 'AstalInhibitManager'],
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
