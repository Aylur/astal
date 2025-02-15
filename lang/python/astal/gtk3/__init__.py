import gi

gi.require_version("Gtk", "3.0")
gi.require_version("Gdk", "3.0")
gi.require_version("Astal", "3.0")

from .app import App
from .astalify import astalify
from .widget import Widget

from gi.repository import Gtk, Gdk, Astal

