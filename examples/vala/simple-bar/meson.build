project('simple-bar', 'vala', 'c')

sources = files(
  'widget/Bar.vala',
  'app.vala',
)

pkgconfig_deps = [
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('gtk+-3.0'),
  dependency('astal-0.1'),
  dependency('astal-battery-0.1'),
]

# needed for GLib.Math
deps = pkgconfig_deps + meson.get_compiler('c').find_library('m')

executable(
  'simple-bar',
  sources,
  dependencies: deps,
)
