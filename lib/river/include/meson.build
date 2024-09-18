
config = configure_file(
  input: 'astal-river.h.in',
  output: 'astal-river.h',
  configuration: {
    'VERSION': meson.project_version(),
    'MAJOR_VERSION': version_split[0],
    'MINOR_VERSION': version_split[1],
    'MICRO_VERSION': version_split[2],
  },
)


astal_river_inc = include_directories('.')
astal_river_headers = config

install_headers(astal_river_headers)
