project(
  'wayland-glib',
  'vala',
  'c',
  version: run_command('cat', join_paths(meson.project_source_root(), 'version')).stdout().strip(),
  meson_version: '>= 0.62.0',
  default_options: [
    'warning_level=2',
    'werror=false',
    'c_std=gnu11',
  ],
)

version_split = meson.project_version().split('.')

deps = [
  dependency('glib-2.0'),
  dependency('gio-2.0'),
  dependency('gobject-2.0'),
  dependency('wayland-client'),
]

sources = [
  'wl-source.vala',
]

lib = static_library(
  meson.project_name(),
  sources,
  dependencies: deps,
  vala_header: meson.project_name() + '.h',
  vala_vapi: meson.project_name() + '.vapi',
)

wayland_glib = declare_dependency(
  link_with: lib,
  include_directories: include_directories('.')
  )


