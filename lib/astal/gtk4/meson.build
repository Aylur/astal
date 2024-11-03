project(
  'astal-4',
  'vala',
  'c',
  version: run_command('cat', join_paths(meson.project_source_root(), 'version')).stdout().strip(),
  meson_version: '>= 0.62.0',
  default_options: [
    'warning_level=2',
    'werror=false',
    'c_std=gnu11',
  ],
)

libdir = get_option('prefix') / get_option('libdir')
pkgdatadir = get_option('prefix') / get_option('datadir') / 'astal'
girpy = files('gir.py')

subdir('src')
