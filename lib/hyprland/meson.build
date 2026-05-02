project(
  'astal-hyprland',
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

assert(
  get_option('lib') or get_option('cli'),
  'Either lib or cli option must be set to true.',
)

version = meson.project_version()
name = meson.project_name()
version_split = version.split('.')
api_version = '0.1'
gir_name = 'AstalHyprland-0.1.gir'
typelib_name = 'AstalHyprland-0.1.typelib'

config = configure_file(
  input: 'src/config.vala.in',
  output: 'config.vala',
  configuration: {
    'API_VERSION': api_version,
    'VERSION': version,
    'MAJOR_VERSION': version_split[0],
    'MINOR_VERSION': version_split[1],
    'MICRO_VERSION': version_split[2],
  },
)

deps = [
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('gio-2.0'),
  dependency('gio-unix-2.0'),
  dependency('json-glib-1.0'),
]

sources = [config] + files(
  'src/client.vala',
  'src/hyprland.vala',
  'src/monitor.vala',
  'src/structs.vala',
  'src/workspace.vala',
  'src/group.vala',
)

if get_option('lib')
  lib = library(
    name,
    sources,
    dependencies: deps,
    vala_args: ['--vapi-comments'],
    vala_header: name + '.h',
    vala_vapi: name + '-' + api_version + '.vapi',
    version: version,
    install: true,
    install_dir: [true, true, true],
  )

  gir = custom_target(
    gir_name,
    command: [
      find_program('python3'),
      files('gir.py'),
      '--name', name,
      '--gir', gir_name,
      '--pkgs', ':'.join(
        'gio-unix-2.0',
        'json-glib-1.0',
      ),
      '--property', ':'.join(
        'Hyprland.binds',
        'Hyprland.monitors',
        'Hyprland.workspaces',
        'Hyprland.clients',
        'Hyprland.groups',
      ),
      sources,
    ],
    input: sources,
    depends: lib,
    output: gir_name,
    install: true,
    install_dir: get_option('datadir') / 'gir-1.0',
  )

  custom_target(
    typelib_name,
    command: [
      find_program('g-ir-compiler'),
      '--output', '@OUTPUT@',
      '--shared-library', get_option('prefix') / get_option('libdir') / '@PLAINNAME@',
      meson.current_build_dir() / gir_name,
    ],
    input: lib,
    output: typelib_name,
    depends: [lib, gir],
    install: true,
    install_dir: get_option('libdir') / 'girepository-1.0',
  )

  import('pkgconfig').generate(
    lib,
    name: name,
    filebase: name + '-' + api_version,
    version: version,
    subdirs: name,
    requires: deps,
    install_dir: get_option('libdir') / 'pkgconfig',
  )
endif

if get_option('cli')
  executable(
    name,
    ['src/cli.vala', sources],
    dependencies: deps,
    install: true,
  )
endif
