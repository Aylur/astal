
config = configure_file(
  input: 'astal-auth.h.in',
  output: 'astal-auth.h',
  configuration: {
    'VERSION': meson.project_version(),
    'MAJOR_VERSION': version_split[0],
    'MINOR_VERSION': version_split[1],
    'MICRO_VERSION': version_split[2],
  },
)


astal_auth_inc = include_directories('.')
astal_auth_headers = config

install_headers(astal_auth_headers)
