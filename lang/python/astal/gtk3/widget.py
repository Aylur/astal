from astal.gtk3.astalify import astalify
from enum import Enum

from gi.repository import Astal, Gtk

class CallableEnum(Enum):
    def __call__(self, *args, **kwargs):
        return self.value(*args, **kwargs)

class Widget(CallableEnum):
    Box = astalify(Astal.Box)
    Button = astalify(Astal.Button)
    CenterBox = astalify(Astal.CenterBox)
    CircularProgress = astalify(Astal.CircularProgress)
    DrawingArea = astalify(Gtk.DrawingArea)
    Entry = astalify(Gtk.Entry)
    EventBox = astalify(Astal.EventBox)
    Icon = astalify(Astal.Icon)
    Label = astalify(Gtk.Label)
    LevelBar = astalify(Astal.LevelBar)
    MenuButton = astalify(Gtk.MenuButton)
    Overlay = astalify(Astal.Overlay)
    Revealer = astalify(Gtk.Revealer)
    Scrollable = astalify(Astal.Scrollable)
    Slider = astalify(Astal.Slider)
    Stack = astalify(Astal.Stack)
    Switch = astalify(Gtk.Switch)
    Window = astalify(Astal.Window)
