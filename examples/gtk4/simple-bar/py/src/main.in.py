#!@PYTHON@

import gi

gi.require_version("Gio", "2.0")
gi.require_version("GObject", "2.0")
gi.require_version("GLib", "2.0")
gi.require_version("Gtk", "4.0")
gi.require_version("Astal", "4.0")

gi.require_version("AstalBattery", "0.1")
gi.require_version("AstalWp", "0.1")
gi.require_version("AstalNetwork", "0.1")
gi.require_version("AstalMpris", "0.1")
gi.require_version("AstalPowerProfiles", "0.1")
gi.require_version("AstalTray", "0.1")
gi.require_version("AstalBluetooth", "0.1")

from gi.repository import Gio
from sys import argv, path
from ctypes import CDLL

CDLL("@LAYER_SHELL_PREFIX@/lib/libgtk4-layer-shell.so")
path.insert(1, "@PKGDATADIR@")
Gio.Resource.load("@PKGDATADIR@/data.gresource")._register()


if __name__ == "__main__":
    from app.App import App

    App.main(argv)
