from gi.repository import Astal, Gtk
from .binding import Binding, kebabify


def set_child(self, child):
    if isinstance(self, Gtk.Bin):
        self.remove(self.get_child())
    if isinstance(self, Gtk.Container):
        self.add(child)


def astalify(ctor):
    ctor.set_css = Astal.widget_set_css
    ctor.get_css = Astal.widget_get_css

    ctor.set_class_name = lambda self, names: Astal.widget_set_class_names(self, names.split())
    ctor.get_class_name = lambda self: " ".join(Astal.widget_set_class_names(self))

    ctor.set_cursor = Astal.widget_set_cursor
    ctor.get_cursor = Astal.widget_get_cursor

    def widget(**kwargs):
        args = {}
        bindings = {}
        handlers = {}
        setup = None
        if not hasattr(kwargs, "visible"):
            kwargs["visible"] = True

        for key, value in kwargs.items():
            if key == "setup":
                setup = value
            if isinstance(value, Binding):
                bindings[key] = value
            if key.startswith("on_"):
                handlers[key] = value
            else:
                args[key] = value

        self = ctor(**args)

        for key, value in bindings.items():
            setter = getattr(self, f"set_{key}")
            setter(value.get())
            unsub = value.subscribe(setter)
            self.connect("destroy", lambda _: unsub())

        for key, value in handlers.items():
            self.connect(kebabify(key.replace("on_", "")), value)

        if setup:
            setup(self)

        return self

    return widget


Box = astalify(Astal.Box),
Button = astalify(Astal.Button),
CenterBox = astalify(Astal.CenterBox),
# TODO: CircularProgress
DrawingArea = astalify(Gtk.DrawingArea),
Entry = astalify(Gtk.Entry),
EventBox = astalify(Astal.EventBox),
# TODO: Fixed
# TODO: FlowBox
Icon = astalify(Astal.Icon),
Label = astalify(Gtk.Label),
LevelBar = astalify(Astal.LevelBar),
# TODO: ListBox
Overlay = astalify(Astal.Overlay),
Revealer = astalify(Gtk.Revealer),
Scrollable = astalify(Astal.Scrollable),
Slider = astalify(Astal.Slider),
# TODO: Stack
Switch = astalify(Gtk.Switch),
Window = astalify(Astal.Window),
