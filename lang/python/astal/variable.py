import gi
import asyncio
from .process import *
from .binding import Binding

gi.require_version("Astal", "3.0")
gi.require_version("AstalIO", "0.1")
gi.require_version("GObject", "2.0")

from gi.repository import Astal, AstalIO, GObject

import threading
import time
from typing import Any, Callable, List, Optional, Union

class Variable(AstalIO.VariableBase):
    def __init__(self, init_value=None):
        super().__init__()
        self.value = init_value
        self.watch_proc = None
        self.poll_interval = None
        self.poll_exec = None
        self.poll_transform = None
        self.poll_fn = None
        self.poll_timer = None
        self.watch_transform = None
        self.watch_exec = []

        self.connect('dropped', self._on_dropped)

    def __del__(self):
        self.emit_dropped()

    def __call__(self, transform: Callable = lambda x: x):
        return Binding(self).transform(transform)

    def subscribe(self, callback):
        id = self.emitter.connect(
            'changed',
            lambda gobject, _=None: callback(self.emitter.get_value())
        )

        def unsubscribe(_=None):
            self.emit_dropped()
            self.emitter.disconnect(id)

        return unsubscribe

    def get_value(self): 
        return self.value
    
    def set_value(self, new_value):
        self.value = new_value
        self.emit_changed()

    def get(self): 
        return self.value
    
    def set(self, new_value):
        self.value = new_value
        self.emit_changed()

    def poll(self, interval, exec, transform=lambda x: x):
        self.stop_poll()
        self.poll_transform = transform
        self.poll_interval = interval
        if isinstance(exec, Callable):
            self.poll_fn = exec
        
        else:
            self.poll_exec = exec

        self.start_poll()
        return self

    def start_poll(self):
        if self.is_polling(): return
        if not self.poll_transform: return

        if self.poll_fn:
            self.poll_timer = AstalIO.Time.interval(self.poll_interval, lambda: self.set_value(self.poll_transform(self.poll_fn(self.get_value()))))

        else:
            self.poll_timer = AstalIO.Time.interval(
                self.poll_interval,
                lambda: exec_async(self.poll_exec, lambda out, err=None: 
                    self.set_value(self.poll_transform(out, self.get_value()))
                )
            )

    def stop_poll(self):
        if self.is_polling():
            self.poll_timer.cancel()
            self.poll_timer = None

        return self

    def watch(self, exec, transform=lambda x, _: x):
        self.stop_watch()
        self.watch_transform = transform
        self.watch_exec = exec
        self.start_watch()
        return self

    def start_watch(self):
        if self.is_watching(): return 
        if not self.watch_transform: return

        self.watch_proc = subprocess(
            self.watch_exec, 
            lambda _, out: self.set_value(self.watch_transform(out, self.get_value())),
        )

    def stop_watch(self):
        if self.is_watching():
            self.watch_proc.kill()
            self.watch_proc = None

        return self

    def _on_dropped(self, *_):
        if self.is_polling():
            self.stop_poll()
        if self.is_watching():
            self.stop_watch()

    def is_polling(self):
        return self.poll_timer != None

    def is_watching(self):
        return self.watch_proc != None

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
