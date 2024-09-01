project(
  'astal_auth',
  'c',
  version: run_command('cat', join_paths(meson.project_source_root(), 'version')).stdout().strip(),
  default_options: [
    'c_std=gnu11',
    'warning_level=3',
    'prefix=/usr',
  ],
)

add_project_arguments(['-Wno-pedantic'], language: 'c')

version_split = meson.project_version().split('.')
lib_so_version = version_split[0] + '.' + version_split[1]

pkg_config = import('pkgconfig')
gnome = import('gnome')

subdir('include')
subdir('src')

install_data('pam/astal-auth', install_dir: get_option('sysconfdir') / 'pam.d')
