
config = configure_file(
  input: 'astal-clipboard.h.in',
  output: 'astal-clipboard.h',
  configuration: {
    'VERSION': meson.project_version(),
    'MAJOR_VERSION': version_split[0],
    'MINOR_VERSION': version_split[1],
    'MICRO_VERSION': version_split[2],
  },
)


astal_clipboard_inc = include_directories('.')
astal_clipboard_headers = config

install_headers(astal_clipboard_headers)
