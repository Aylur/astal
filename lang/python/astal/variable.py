from astal.process import exec_async, subprocess
from astal.binding import Binding
from astal.time import interval

from gi.repository import AstalIO, GObject

from typing import Callable, List, Any, Union

class Variable(AstalIO.VariableBase):
    _value: Any

    _poll: AstalIO.Time | None = None
    _watch: AstalIO.Process | None = None

    _poll_interval: int = 1000
    _poll_exec: List[str] | str = ""
    _poll_transform: Callable[[Any, Any], Any] | Callable[[Any], Any] = lambda x, _ = None: x
    _poll_fn: Callable[[Any], Any] | None = lambda last: None

    _watch_transform: Callable[[Any, Any], Any] = lambda x, _ = None: x
    _watch_exec: List[str] | str = ""

    def __init__(self, value=None):
        super().__init__()

        self.value = value

        self.connect('dropped', self._on_dropped)

    def __del__(self):
        self.emit_dropped()

    def __call__(self, transform: Callable = lambda x: x):
        return Binding(self).transform(transform)

    def get(self): 
        return self.value
    
    def set(self, new_value):
        self.value = new_value
        self.emit_changed()

    def is_polling(self):
        return self._poll != None

    def is_watching(self):
        return self._watch != None

    def start_poll(self):
        if self.is_polling(): return

        if self._poll_fn:
            self._poll = interval(
                self._poll_interval, 
                lambda: self.set(self._poll_transform(self._poll_fn(self.get()))))

        else:
            self._poll = interval(
                self._poll_interval,
                lambda: exec_async(
                    self._poll_exec, lambda out, err=None: 
                        self.set(self._poll_transform(out, self.get()))))

    def start_watch(self):
        if self.is_watching(): return 

        self._watch = subprocess(
            self._watch_exec, 
            lambda _, out: self.set(self._watch_transform(out, self.get())))

    def stop_poll(self):
        if self.is_polling():
            self._poll.cancel()

        self._poll = None

    def stop_watch(self):
        if self.is_watching():
            self._watch.kill()
            
        self._watch = None

    def poll(self, interval: int, exec: str | Callable, transform: Callable[[Any, Any], Any] = lambda x, _ = None: x):
        self.stop_poll()
        self._poll_interval = interval
        self._poll_transform = transform

        if isinstance(exec, Callable):
            self._poll_fn = exec
            self._poll_exec = ""

        else:
            self._poll_exec = exec
            self._poll_fn = None

        self.start_poll()

        return self

    def watch(self, exec: str, transform: Callable[[Any, Any], Any] = lambda x, _ = None: x):
        self.stop_watch()
        self._watch_exec = exec
        self._watch_transform = transform
        self.start_watch()

        return self

    def drop(self):
        self.emit_dropped()

    def _on_dropped(self, *_):
        self.stop_poll()
        self.stop_watch()

    def subscribe(self, callback):
        id = self.connect(
            'changed',
            lambda gobject, _=None: callback(self.get())
        )

        def unsubscribe(_=None):
            self.emit_dropped()
            self.disconnect(id)

        return unsubscribe

    def observe(self, object, signal_or_callback, callback=lambda _, x: x):
        if isinstance(signal_or_callback, str):
            f = callback
        
        else:
            f = signal_or_callback

        set = lambda *args: self.set(f(*args))

        if isinstance(signal_or_callback, str):
            object.connect(signal_or_callback, set)

        if isinstance(object, list):
            for connectable, signal in object:
                connectable.connect(signal, set)

        return self

    @classmethod
    def derive(cls, objects: List[Union[Binding, 'Variable']], transform=lambda *args: args):
        update = lambda: transform(*map(lambda object: object.get(), objects))

        derived = Variable(update())

        unsubs = [*map(lambda object: object.subscribe(lambda *_: derived.set(update())), objects)]

        derived.connect('dropped', lambda *_: map(lambda unsub: unsub(), unsubs))

        return derived
