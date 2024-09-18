
config = configure_file(
  input: 'astal-wp.h.in',
  output: 'astal-wp.h',
  configuration: {
    'VERSION': meson.project_version(),
    'MAJOR_VERSION': version_split[0],
    'MINOR_VERSION': version_split[1],
    'MICRO_VERSION': version_split[2],
  },
)

astal_wireplumber_inc = include_directories('.', 'astal/wireplumber', 'private')
astal_wireplumber_headers = config

install_headers(astal_wireplumber_headers)

subdir('astal/wireplumber')
