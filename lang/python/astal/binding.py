from gi.repository import GObject

class Binding:
    def __init__(self, emitter: GObject.GObject, property: str | None = None, transform_fn = lambda x: x):
        self.emitter = emitter
        self.property = property
        self.transform_fn = transform_fn

    def get(self):
        return self.transform_fn(self.emitter.get_property(self.property) if self.property else self.emitter.get())

    def transform(self, fn):
        return Binding(self.emitter, self.property, lambda x: fn(self.transform_fn(x)))

    def subscribe(self, callback):
        id = self.emitter.connect(
            f'notify::{self.property}' if self.property else 'changed',
            lambda *_: callback(self.transform_fn(self.emitter.get_property(self.property) if self.property else self.emitter.get()))
        )

        def unsubscribe(_=None):
            self.emitter.disconnect(id)

        return unsubscribe

def bind(*args, **kwargs):
    return Binding(*args, **kwargs)
