project(
  'astal_river',
  'c',
  version: '0.1.0',
  default_options: ['c_std=gnu11', 'warning_level=3', 'prefix=/usr'],
)

add_project_arguments(['-Wno-pedantic', '-Wno-unused-parameter'], language: 'c')

version_split = meson.project_version().split('.')
lib_so_version = version_split[0] + '.' + version_split[1]

pkg_config = import('pkgconfig')
gnome = import('gnome')

subdir('protocols')
subdir('include')
subdir('src')
