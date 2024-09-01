srcs = files(
  'pam.c',
)

deps = [dependency('gobject-2.0'), dependency('gio-2.0'), dependency('pam')]

astal_auth_lib = library(
  'astal-auth',
  sources: srcs,
  include_directories: astal_auth_inc,
  dependencies: deps,
  version: meson.project_version(),
  install: true,
)

libastal_auth = declare_dependency(link_with: astal_auth_lib, include_directories: astal_auth_inc)

executable(
  'astal-auth',
  files('astal-auth.c'),
  dependencies: [dependency('gobject-2.0'), libastal_auth],
  install: true,
)

pkg_config_name = 'astal-auth-' + lib_so_version

if get_option('introspection')
  gir = gnome.generate_gir(
    astal_auth_lib,
    sources: srcs + astal_auth_headers,
    nsversion: '0.1',
    namespace: 'AstalAuth',
    symbol_prefix: 'astal_auth',
    identifier_prefix: 'AstalAuth',
    includes: ['GObject-2.0', 'Gio-2.0'],
    header: 'astal-auth.h',
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
  name: 'astal-auth',
  version: meson.project_version(),
  libraries: [astal_auth_lib],
  filebase: pkg_config_name,
  subdirs: 'astal',
  description: 'astal authentication module',
  url: 'https://github.com/astal-sh/auth',
)
