project('simple-bar', 'vala', 'c')

bindir = get_option('prefix') / get_option('bindir')
libdir = get_option('prefix') / get_option('libdir')

pkgconfig_deps = [
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('gtk+-3.0'),
  dependency('libnm'),
  dependency('astal-io-0.1'),
  dependency('astal-3.0'),
  dependency('astal-battery-0.1'),
  dependency('astal-wireplumber-0.1'),
  dependency('astal-network-0.1'),
  dependency('astal-tray-0.1'),
  dependency('astal-mpris-0.1'),
  dependency('astal-hyprland-0.1'),
]

# needed for GLib.Math
deps = pkgconfig_deps + meson.get_compiler('c').find_library('m')

main = configure_file(
  input: 'app.in.vala',
  output: 'app.vala',
  configuration: {
    'STYLE': run_command(
      find_program('sass'),
      meson.project_source_root() / 'style.scss',
    ).stdout(),
  },
)

sources = files(
  'widget/Bar.vala',
)

executable(
  'simple-bar',
  [sources, main],
  dependencies: deps,
  install: true,
  install_dir: bindir,
)
