project(
  'astal',
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

prefix = get_option('prefix')
libdir = get_option('prefix') / get_option('libdir')
pkgdatadir = prefix / get_option('datadir') / 'astal'


assert(
  get_option('lib') or get_option('cli'),
  'Either lib or cli option must be set to true.',
)

if get_option('gjs')
  install_subdir('gjs', install_dir: pkgdatadir)
endif

subdir('src')
