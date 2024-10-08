import gi

gi.require_version("Astal", "0.1")
gi.require_version("Gtk", "3.0")
gi.require_version("Gdk", "3.0")
gi.require_version("GLib", "2.0")
gi.require_version("Gio", "2.0")
gi.require_version("GObject", "2.0")

from gi.repository import Astal, Gtk, GLib, Gio, GObject, Gdk, GObject
from astal.application import App
from astal.variable import Variable
import astal.widget as Widget
from astal.binding import Binding as bind

__all__ = [
    "App",
    "Variable",
    "Widget",
    "bind",
    "Astal",
    "Gtk",
    "Gdk",
    "GLib",
    "Gio",
    "GObject",
]
