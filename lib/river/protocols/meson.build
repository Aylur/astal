
fs = import('fs')

wl_vapi_gen_prog = find_program('wl-vapi-gen', required: false)
if not wl_vapi_gen_prog.found()
  subproject('wl-vapi-gen')
  wl_vapi_gen_prog = find_program('wl-vapi-gen', required: false)
endif

# subproject('wl-vapi-gen')
wayland_protos = dependency('wayland-protocols')
wayland_scanner = find_program('wayland-scanner')
wayland_client_dep = dependency('wayland-client')

wl_protocol_dir = wayland_protos.get_variable(pkgconfig: 'pkgdatadir')

protocols = [
  'river-status-unstable-v1.xml',
  'river-layout-v3.xml',
  'river-control-unstable-v1.xml'
]

protocol_deps = []
protocol_vapis = []

foreach protocol : protocols
  client_header = custom_target(
    fs.name(protocol) + '_header',
    input: protocol,
    output: '@BASENAME@.h',
    command: [
      wayland_scanner,
      '-c', 'client-header',
      '@INPUT@',
      '@OUTPUT@',
    ],
  )

  code = custom_target(
    fs.name(protocol) + '_code',
    input: protocol,
    output: '@BASENAME@.c',
    command: [
      wayland_scanner,
      '-c', 'private-code',
      '@INPUT@',
      '@OUTPUT@',
    ],
  )

  vapi = custom_target(
    fs.name(protocol) + '_vapi',
    input: [protocol, client_header],
    output: '@BASENAME0@.vapi',
    command: [
      wl_vapi_gen_prog,
      '--vapi', '@OUTPUT@',
      '--protocol', '@INPUT0@',
      '--cheader', '@INPUT1@',
    ],
  )

  protocol_deps += declare_dependency(
    dependencies: [wayland_client_dep],
    sources: [client_header, code, vapi],
  )
  protocol_vapis += vapi
endforeach
