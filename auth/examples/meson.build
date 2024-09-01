
deps_example = [
    dependency('gobject-2.0'),
    dependency('libbsd'),
    libastal_auth
]

astal_auth_full_exmple = executable(
    'astal_auth_full_example',
    files('full_example.c'),
    dependencies : deps_example,
    install : false)

astal_auth_simple_example = executable(
    'astal_auth_simple_example',
    files('simple_example.c'),
    dependencies : deps_example,
    install : false)
