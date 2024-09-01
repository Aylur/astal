wayland_scanner = find_program('wayland-scanner')

protocols = ['river-status-unstable-v1.xml', 'river-control-unstable-v1.xml']

gen_client_header = generator(
  wayland_scanner,
  output: ['@BASENAME@-client.h'],
  arguments: ['-c', 'client-header', '@INPUT@', '@BUILD_DIR@/@BASENAME@-client.h'],
)

gen_private_code = generator(
  wayland_scanner,
  output: ['@BASENAME@.c'],
  arguments: ['-c', 'private-code', '@INPUT@', '@BUILD_DIR@/@BASENAME@.c'],
)

client_protocol_srcs = []

foreach protocol : protocols
  client_header = gen_client_header.process(protocol)
  code = gen_private_code.process(protocol)
  client_protocol_srcs += [client_header, code]
endforeach
