srcs = files(
  'clipboard.c',
  'selection.c'
)

deps = [
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
  dependency('gdk-pixbuf-2.0'),
  dependency('wayland-client'),
  wayland_glib_dep
]

astal_clipboard_lib = library(
  'astal-clipboard',
  sources: srcs + client_protocol_srcs,
  include_directories: astal_clipboard_inc,
  dependencies: deps,
  version: meson.project_version(),
  install: true,
)

pkg_config_name = 'astal-clipboard-' + lib_so_version

if get_option('introspection')
  gir = gnome.generate_gir(
    astal_clipboard_lib,
    sources: srcs + astal_clipboard_headers,
    nsversion: '0.1',
    namespace: 'AstalClipboard',
    symbol_prefix: 'astal_clipboard',
    identifier_prefix: 'AstalClipboard',
    includes: ['GObject-2.0', 'Gio-2.0', 'GdkPixbuf-2.0'],
    header: 'astal-clipboard.h',
    export_packages: pkg_config_name,
    install: true,
  )

  if get_option('vapi')
    gnome.generate_vapi(
      pkg_config_name,
      sources: [gir[0]],
      packages: ['gobject-2.0', 'gio-2.0'],
      install: true,
    )
  endif
endif

pkg_config.generate(
  name: 'astal-clipboard',
  version: meson.project_version(),
  libraries: [astal_clipboard_lib],
  filebase: pkg_config_name,
  subdirs: 'astal',
  description: 'astal clipboard library',
  url: 'https://github.com/aylur/astal/lib/clipboard',
)
