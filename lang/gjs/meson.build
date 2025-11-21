project('astal-gjs')

dest = get_option('prefix') / get_option('datadir') / 'astal' / 'gjs'

dependency('astal-io-0.1')

gtk3 = dependency('astal-3.0', required: false)
gtk4 = dependency('astal-4-4.0', required: false)

if (not gtk3.found() and not gtk4.found())
  error('Neither astal-3.0 nor astal-4.0 was found.')
endif

install_data(
  [
    'src/_app.ts',
    'src/_astal.ts',
    'src/binding.ts',
    'src/file.ts',
    'src/gobject.ts',
    'src/index.ts',
    'src/overrides.ts',
    'src/process.ts',
    'src/time.ts',
    'src/variable.ts',
    'src/package.json',
  ],
  install_dir: dest,
)

install_subdir('src/gtk3', install_dir: dest)
install_subdir('src/gtk4', install_dir: dest)

import('pkgconfig').generate(
  description: 'Astal GJS pacakge',
  name: meson.project_name(),
  install_dir: get_option('libdir') / 'pkgconfig',
  variables: {
    'srcdir': dest,
  },
)
