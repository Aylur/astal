from functools import partial

from typing import Callable

from astal.binding import Binding
from astal.variable import Variable

from gi.repository import Astal, Gtk, GObject

def astalify(widget: Gtk.Widget):
    class Widget(widget):
        __gtype_name__ = "AstalPy" + widget.__name__
        class_name = ''

        def hook(self, object: GObject.Object, signal_or_callback: str | Callable, callback: Callable = lambda _, x: x):
            if isinstance(signal_or_callback, Callable):
                callback = signal_or_callback

            if isinstance(object, Variable):
                unsubscribe = object.subscribe(callback)

            else:
                if isinstance(signal_or_callback, Callable): return 

                if 'notify::' in signal_or_callback:
                    id = object.connect(
                        f'{signal_or_callback}', 
                        lambda *_: callback(
                            self, 
                            object.get_property(signal_or_callback.replace('notify::', '').replace('-', '_'))))

                else:
                    id = object.connect(
                        signal_or_callback, 
                        lambda _, value, *args: callback(self, value) if not args else callback(self, value, *args))

                unsubscribe = lambda *_: object.disconnect(id)

            self.connect('destroy', unsubscribe)
        
        def _set_children(self, children):
            children = map(lambda x: x if isinstance(x, Gtk.Widget) else Gtk.Label(visible=True, label=x), children)

            if isinstance(self, Gtk.Container):
                for child in children:
                    self.add(child)

            else:
                raise TypeError(f"{self.__gtype_name__} is not an instance of Gtk.Container")

        def toggle_class_name(self, name: str, state: bool | None = None):
            Astal.widget_toggle_class_name(self, name, state if state is not None else not name in Astal.widget_get_class_names(self))

        @GObject.Property(type=str)
        def class_name(self):
            return ' '.join(Astal.widget_get_class_names(self))

        @class_name.setter
        def class_name(self, name):
            Astal.widget_set_class_names(self, name.split(' '))

        @GObject.Property(type=str)
        def css(self):
            return Astal.widget_get_css(self)

        @css.setter
        def css(self, css: str):
            Astal.widget_set_css(self, css)

        @GObject.Property(type=str)
        def cursor(self):
            return Astal.widget_get_cursor(self)

        @cursor.setter
        def cursor(self, cursor: str):
            Astal.widget_set_cursor(self, cursor)

        @GObject.Property(type=str)
        def click_through(self):
            return Astal.widget_get_click_through(self)
       
        @click_through.setter
        def click_through(self, click_through: str):
            Astal.widget_set_click_through(self, click_through)

        @GObject.Property()
        def children(self):
            if isinstance(self, Gtk.Container):
                return self.get_children()

        @children.setter
        def children(self, children):
            # TODO: inquire as to the purpose of the special case for Gtk.Bin in the lua bindings.
            if isinstance(self, Gtk.Container):
                for child in self.get_children():
                    self.remove(child)

            self._set_children(children)

        def __init__(self, **props):
            super().__init__()

            self.set_visible(props.get("visible", True))

            for prop, value in props.items():
                if isinstance(value, Binding):
                    self.set_property(prop, value.get())
                    unsubscribe = value.subscribe(partial(self.set_property, prop))
                    self.connect('destroy', unsubscribe)

                elif 'on_' == prop[0:3] and isinstance(value, Callable):
                    self.connect(prop.replace('on_', '', 1), value) 

                elif prop.replace('_', '-') in map(lambda x: x.name, self.props):
                    self.set_property(prop.replace('_', '-'), value)

                elif prop == 'setup' and isinstance(value, Callable):
                    value(self)

                else:
                    self.__setattr__(prop, value)

    return Widget
