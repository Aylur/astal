from gi.repository import Astal

from .binding import Binding


class Variable:
    def __init__(self, init):
        v = Astal.Variable.new(init)
        self._variable = v
        self._err_handler = print
        v.connect("error", lambda _, err: self._err_handler(err) if self._err_handler else None)

    def __call__(self, transform=None):
        if transform:
            return Binding(self).as_(transform)

        return Binding(self)

    def __str__(self):
        return f"Variable<{self.get()}>"

    def get(self):
        return self._variable.get_value()

    def set(self, value):
        return self._variable.set_value(value)

    def watch(self, cmd):
        if isinstance(cmd, str):
            self._variable.watch(cmd)
        elif isinstance(cmd, list):
            self._variable.watchv(cmd)
        return self

    def poll(self, interval, cmd):
        if isinstance(cmd, str):
            self._variable.poll(interval, cmd)
        elif isinstance(cmd, list):
            self._variable.pollv(interval, cmd)
        else:
            self._variable.pollfn(interval, cmd)
        return self

    def start_watch(self):
        self._variable.start_watch()

    def start_poll(self):
        self._variable.start_poll()

    def stop_watch(self):
        self._variable.stop_watch()

    def stop_poll(self):
        self._variable.stop_poll()

    def drop(self):
        self._variable.emit_dropped()
        self._variable.run_dispose()

    def on_dropped(self, callback):
        self._variable.connect("dropped", lambda _: callback())
        return self

    def on_error(self, callback):
        self._err_handler = None
        self._variable.connect("error", lambda _, e: callback(e))
        return self

    def subscribe(self, callback):
        s = self._variable.connect("changed", lambda _: callback(self.get()))
        return lambda: self._variable.disconnect(s)

    def observe(self, objs, sigOrFn, callback=None):
        if callable(sigOrFn):
            f = sigOrFn
        elif callable(callback):
            f = callback
        else:
            f = lambda *_: self.get()

        def setter(*args):
            self.set(f(*args))

        if isinstance(objs, list):
            for obj in objs:
                obj[0].connect(obj[1], setter)
        elif isinstance(sigOrFn, str):
            objs.connect(sigOrFn, setter)

        return self

    @staticmethod
    def derive(deps, fn):
        def update():
            return fn(*[d.get() for d in deps])

        derived = Variable(update())
        unsubs = [dep.subscribe(lambda _: derived.set(update())) for dep in deps]
        derived.on_dropped(lambda: ([unsub() for unsub in unsubs]))
        return derived
