srcs = files(
  'river-output.c',
  'river.c',
  'wayland-source.c',
)

deps = [
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
  dependency('wayland-client'),
  dependency('json-glib-1.0'),
]

astal_river_lib = library(
  'astal-river',
  sources: srcs + client_protocol_srcs,
  include_directories: astal_river_inc,
  dependencies: deps,
  version: meson.project_version(),
  install: true,
)

libastal_river = declare_dependency(link_with: astal_river_lib, include_directories: astal_river_inc)

executable(
  'astal-river',
  files('astal-river.c'),
  dependencies: [
    dependency('gobject-2.0'),
    dependency('gio-2.0'),
    dependency('json-glib-1.0'),
    libastal_river,
  ],
  install: true,
)

pkg_config_name = 'astal-river-' + lib_so_version

if get_option('introspection')
  gir = gnome.generate_gir(
    astal_river_lib,
    sources: srcs + astal_river_headers,
    nsversion: '0.1',
    namespace: 'AstalRiver',
    symbol_prefix: 'astal_river',
    identifier_prefix: 'AstalRiver',
    includes: ['GObject-2.0', 'Gio-2.0'],
    header: 'astal-river.h',
    export_packages: pkg_config_name,
    install: true,
  )

  if get_option('vapi')
    gnome.generate_vapi(
      pkg_config_name,
      sources: [gir[0]],
      packages: ['gobject-2.0', 'gio-2.0'],
      install: true,
    )
  endif
endif

pkg_config.generate(
  name: 'astal-river',
  version: meson.project_version(),
  libraries: [astal_river_lib],
  filebase: pkg_config_name,
  subdirs: 'astal',
  description: 'astal riverentication module',
  url: 'https://github.com/astal-sh/river',
)
