import re


def kebabify(string):
    return re.sub(r"([a-z])([A-Z])", r"\1-\2", string).replace("_", "-").lower()


class Binding:
    def __init__(self, emitter, prop=None):
        self.emitter = emitter
        self.prop = kebabify(prop) if prop else None
        self.transform_fn = lambda v: v

    def __str__(self):
        return f"Binding<{self.emitter}{', ' + self.prop if self.prop else ''}>"

    def as_(self, fn):
        bind = Binding(self.emitter, self.prop)
        bind.transform_fn = lambda v: fn(self.transform_fn(v))
        return bind

    def get(self):
        if hasattr(self.emitter, "get") and callable(self.emitter.get):
            return self.transform_fn(self.emitter.get())

        return self.transform_fn(self.emitter[f"get_{self.prop}"]())

    def subscribe(self, callback):
        if hasattr(self.emitter, "subscribe") and callable(self.emitter.subscribe):
            return self.emitter.subscribe(lambda _: callback(self.get()))

        i = self.emitter.connect(f"notify::{self.prop}", lambda: callback(self.get()))
        return lambda: self.emitter.disconnect(i)
