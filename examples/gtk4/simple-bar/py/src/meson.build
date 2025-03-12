pkgdatadir = get_option('prefix') / get_option('datadir') / meson.project_name()
bindir = get_option('prefix') / get_option('bindir')

blp = find_program('blueprint-compiler', required: true)
sass = find_program('sass', required: true)
python = find_program('python3', required: true)
layer_shell = dependency('gtk4-layer-shell-0')

blueprint_sources = files(
  'ui/Bar.blp',
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

# bundle styles
css = custom_target(
  'scss',
  input: files('main.scss'),
  command: [sass, '@INPUT@', '@OUTPUT@'],
  output: 'main.css',
)

# compiling ui and css into a binary
import('gnome').compile_resources(
  'data',
  files('gresource.xml'),
  dependencies: [ui, css],
  gresource_bundle: true,
  install: true,
  install_dir: pkgdatadir,
)

# install python sources
install_subdir('py', install_dir: pkgdatadir)

# configure the main python entry file
configure_file(
  input: 'main.in.py',
  output: meson.project_name(),
  configuration: {
    'PYTHON': python.full_path(),
    'LAYER_SHELL_PREFIX': layer_shell.get_variable('prefix'),
    'PKGDATADIR': pkgdatadir,
  },
  install: true,
  install_dir: bindir,
)
