import sys
from gi.repository import Astal, Gtk, GObject
from astal.binding import Binding, kebabify


def astalify(klass: type[Gtk.Widget]) -> type:
    def _set_children(self, children):
        if not isinstance(children, list):
            children = [children]

        nchildren = [
            ch if isinstance(ch, Gtk.Widget) else Gtk.Label(visible=True, label=str(ch))
            for ch in children
        ]

        # Remove existing child widgets
        if isinstance(self, Gtk.Bin):
            if ch := self.get_child():
                self.remove(ch)
            if ch is not None and ch not in nchildren:
                ch.destroy()

        elif isinstance(self, Gtk.Container):
            for ch in self.get_children():
                self.remove(ch)
                if ch not in nchildren:
                    ch.destroy()

        # Add children based on the type of container
        if isinstance(self, Astal.Box):
            self.set_children(nchildren)

        elif isinstance(self, Astal.Stack):
            self.set_children(nchildren)

        elif isinstance(self, Astal.CenterBox):
            print("Hello")
            self.start_widget = nchildren[0] if len(nchildren) > 0 else None
            self.center_widget = nchildren[1] if len(nchildren) > 1 else None
            self.end_widget = nchildren[2] if len(nchildren) > 2 else None

        elif isinstance(self, Astal.Overlay):
            child, *overlays = nchildren
            self.set_child(child)
            self.set_overlays(overlays)

        elif isinstance(self, Gtk.Container):
            for ch in nchildren:
                self.add(ch)

    def __init__(self, *_, **kwargs) -> None:
        args = {}
        bindings = {}
        handlers = {}
        children = []
        setup = None

        args["visible"] = kwargs.get("visible", True)

        for key, value in kwargs.items():
            if key == "setup":
                setup = value
            elif isinstance(value, Binding):
                bindings[key] = value
            elif key == "child":
                children = [value]
            elif key == "children":
                children = value
            elif key.startswith("on_"):
                handlers[key] = value
            else:
                args[key] = value

        super(Widget, self).__init__(**args)

        if len(children) > 0:
            _set_children(self, children)

        for key, value in bindings.items():
            setter = (
                lambda v: _set_children(self, v)
                if key == "child" or key == "children"
                else getattr(self, f"set_{key}")
            )

            setter(value.get())
            unsub = value.subscribe(setter)
            self.connect("destroy", lambda _: unsub())

        for key, value in handlers.items():
            sig = kebabify(key.replace("on_", ""))
            if isinstance(value, str):

                def callback(_, task):
                    try:
                        print(Astal.Process.exec_finish(task))
                    except Exception as e:
                        print(e, file=sys.stderr)

                self.connect(sig, lambda *_: Astal.Process.exec_async(value, callback))
            else:
                self.connect(kebabify(key.replace("on_", "")), value)

        if setup:
            setup(self)

    def do_get_property(self, prop):
        if prop.name == "class-name":
            return " ".join(Astal.widget_get_class_names(self))
        elif prop.name == "css":
            return Astal.widget_get_css(self)
        elif prop.name == "cursor":
            return Astal.widget_get_cursor(self)
        elif prop.name == "click-through":
            return Astal.widget_get_click_through(self)
        else:
            return super(Widget, self).do_get_property(prop)

    def do_set_property(self, prop, value):
        if prop.name == "class-name":
            return Astal.widget_set_class_names(self, value.split())
        elif prop.name == "css":
            return Astal.widget_set_css(self, value)
        elif prop.name == "cursor":
            return Astal.widget_set_cursor(self, value)
        elif prop.name == "click-through":
            return Astal.widget_set_click_through(self, value)
        else:
            return super(Widget, self).do_set_property(prop, value)

    Widget = type(
        f"AstalPy{klass.__name__}",
        (klass,),
        {
            "__gtype_name__": f"AstalPy{klass.__name__}",
            "__gproperties__": {
                "class-name": (str, "", "", "", GObject.PARAM_READWRITE),
                "css": (str, "", "", "", GObject.PARAM_READWRITE),
                "cursor": (str, "", "", "default", GObject.PARAM_READWRITE),
                "click-through": (bool, "", "", False, GObject.PARAM_READWRITE),
                "implicit-destroy": (bool, "", "", False, GObject.PARAM_READWRITE),
            },
            "__init__": __init__,
            "do_get_property": do_get_property,
            "do_set_property": do_set_property,
        },
    )

    return Widget


Box = astalify(Astal.Box)
Button = astalify(Astal.Button)
CenterBox = astalify(Astal.CenterBox)
CircularProgress = astalify(Astal.CircularProgress)
DrawingArea = astalify(Gtk.DrawingArea)
Entry = astalify(Gtk.Entry)
EventBox = astalify(Astal.EventBox)
# TODO: Fixed
# TODO: FlowBox
Icon = astalify(Astal.Icon)
Label = astalify(Astal.Label)
LevelBar = astalify(Astal.LevelBar)
# TODO: ListBox
Overlay = astalify(Astal.Overlay)
Revealer = astalify(Gtk.Revealer)
Scrollable = astalify(Astal.Scrollable)
Slider = astalify(Astal.Slider)
Stack = astalify(Astal.Stack)
Switch = astalify(Gtk.Switch)
Window = astalify(Astal.Window)
