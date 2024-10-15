project('astal-gjs')

dest = get_option('prefix') / get_option('datadir') / 'astal' / 'gjs'

dependency('astal-io-0.1')
dependency('astal-3.0')

install_data(
  [
    'src/binding.ts',
    'src/file.ts',
    'src/gobject.ts',
    'src/index.ts',
    'src/process.ts',
    'src/time.ts',
    'src/variable.ts',
  ],
  install_dir: dest,
)

install_subdir('src/gtk3', install_dir: dest)
# install_subdir('src/gtk4', install_dir: dest)
