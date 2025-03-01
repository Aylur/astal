pkgdatadir = get_option('prefix') / get_option('datadir')
bindir = get_option('prefix') / get_option('bindir')
blp = find_program('blueprint-compiler', required: true)
sass = find_program('sass', required: true)

dependencies = [
  dependency('gtk4-layer-shell-0'),
  dependency('astal-io-0.1'),
  dependency('glib-2.0'),
  dependency('astal-4-4.0'),
  dependency('astal-battery-0.1'),
  dependency('astal-wireplumber-0.1'),
  dependency('astal-network-0.1'),
  dependency('libnm'),
  dependency('astal-mpris-0.1'),
  dependency('astal-power-profiles-0.1'),
  dependency('astal-tray-0.1'),
  dependency('astal-bluetooth-0.1'),
]

blueprint_sources = files(
  'ui/Bar.blp',
)

vala_sources = files(
  'vala/Bar.vala',
  'vala/App.vala',
)

# transplie blueprints
ui = custom_target(
  'blueprint',
  input: blueprint_sources,
  output: '.',
  command: [
    blp,
    'batch-compile',
    '@OUTPUT@',
    '@CURRENT_SOURCE_DIR@',
    '@INPUT@',
  ],
)

# bundle scss files
css = custom_target(
  'scss',
  input: files('main.scss'),
  command: [sass, '@INPUT@', '@OUTPUT@'],
  output: ['main.css'],
)

# compiling data files into a binary
resource = import('gnome').compile_resources(
  'data',
  files('gresource.xml'),
  dependencies: [ui, css],
  source_dir: meson.current_build_dir(),
)

executable(
  meson.project_name(),
  dependencies: dependencies,
  sources: [vala_sources, resource],
  link_args: ['-lm'], # Link math library
  install: true,
  install_dir: bindir,
)
