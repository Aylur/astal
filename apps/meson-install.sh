#!/usr/bin/env bash

meson setup \
	--prefix /usr \
	--libexecdir lib \
	--sbindir bin \
	--buildtype plain \
	--auto-features enabled \
	--wrap-mode nodownload \
	-D b_lto=false \
	-D b_pie=true \
	-D python.bytecompile=1 \
	--wipe \
	build

meson install -C build
