project(
  'astal-hyprland-focus-grab',
  'vala',
  'c',
  meson_version: '>= 1.5.0',
  version: run_command('cat', join_paths(meson.project_source_root(), 'version')).stdout().strip(),
  default_options: ['warning_level=2', 'werror=false', 'c_std=gnu11'],
)

version = meson.project_version()
name = meson.project_name()
version_split = version.split('.')
api_version = '0.1'
gir_name_base = 'AstalHyprlandFocusGrab'

subdir('protocols')

sources = files('src/context.vala', 'src/grab.vala', 'src/manager.vala')

deps = [
  dependency('glib-2.0'),
  dependency('gobject-2.0'),
  dependency('astal-wl-0.1'),
  dependency('wayland-client'),
]

build_configs = []

gtk_version = get_option('gtk-version')
if '3' in gtk_version
  build_configs += {
    'define': 'GTK_3',
    'name': name + '-3',
    'version': '3.0',
    'wl_shim': files('src/window-surface-gtk3.c'),
    'extra_deps': [dependency('gtk+-3.0')],
    'gir_deps': ['gtk+-3.0'],
  }
endif

if '4' in gtk_version
  build_configs += {
    'define': 'GTK_4',
    'name': name + '-4',
    'version': '4.0',
    'wl_shim': files('src/window-surface-gtk4.c'),
    'extra_deps': [
      dependency('gtk4'),
      dependency('gtk4-wayland'),
    ],
    'gir_deps': ['gtk4', 'gtk4-wayland'],
  }
endif

foreach config : build_configs
  config_vala = configure_file(
    input: 'src/config.vala.in',
    output: 'config.vala',
    configuration: {
      'API_VERSION': config['version'],
      'VERSION': version,
      'MAJOR_VERSION': version_split[0],
      'MINOR_VERSION': version_split[1],
      'MICRO_VERSION': version_split[2],
    },
  )

  lib = library(
    config['name'],
    sources + config_vala + config['wl_shim'],
    dependencies: deps + protocol_deps + config['extra_deps'],
    vala_args: ['--vapi-comments', '-D', config['define']],
    vala_header: config['name'] + '.h',
    vala_vapi: config['name'] + '-' + api_version + '.vapi',
    version: config['version'],
    install: true,
    install_dir: [true, true, true],
  )

  gir_name = gir_name_base + '-' + config['version'] + '.gir'
  gir = custom_target(
    gir_name,
    command: [
      find_program('python3'),
      files('gir.py'),
      '--name', name,
      '--gir', gir_name,
      '--pkgs', ':'.join(
        [
          'wayland-client',
          'astal-wl-0.1',
        ]
        + config['gir_deps'],
      ),
      '@INPUT@',
    ],
    input: sources + protocol_vapis,
    depends: lib,
    output: gir_name,
    install: true,
    install_dir: get_option('datadir') / 'gir-1.0',
  )

  typelib_name = gir_name_base + '-' + config['version'] + '.typelib'
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
    name: config['name'],
    filebase: config['name'] + '-' + api_version,
    version: version,
    subdirs: config['name'],
    requires: deps,
    install_dir: get_option('libdir') / 'pkgconfig',
  )
endforeach
