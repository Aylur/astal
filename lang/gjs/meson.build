project('astal-gjs')

datadir = get_option('prefix') / get_option('datadir')
pkgdata = datadir / 'astal' / 'gjs'

install_data('index.ts', install_dir: pkgdata)
install_subdir('lib', install_dir: pkgdata)
install_subdir('gtk3', install_dir: pkgdata)
install_subdir('gtk4', install_dir: pkgdata)
