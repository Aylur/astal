project(
  'astal-tray',
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

assert(
  get_option('lib') or get_option('cli'),
  'Either lib or cli option must be set to true.',
)

subdir('src')
