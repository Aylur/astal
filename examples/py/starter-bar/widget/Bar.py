from gi.repository import Astal, Gtk, Gdk, GLib


class Time(Astal.Label):
    def __init__(self, format="%H:%M:%S"):
        super().__init__(visible=True)
        self.connect("destroy", self.on_destroy)
        self.format = format
        self.time = Astal.Time.interval(1000, self.on_tick)

    def on_tick(self):
        datetime = GLib.DateTime.new_now_local()
        assert datetime
        time = datetime.format(self.format)
        assert time
        self.set_label(time)

    def on_destroy(self, *args, **kwargs):
        self.time.cancel()


class Bar(Astal.Window):
    def __init__(self, monitor: Gdk.Monitor):
        super().__init__(
            visible=True,
            gdkmonitor=monitor,
            name="Bar" + str(monitor.get_model()),
            anchor=Astal.WindowAnchor.LEFT
            | Astal.WindowAnchor.RIGHT
            | Astal.WindowAnchor.TOP,
            exclusivity=Astal.Exclusivity.EXCLUSIVE,
        )

        Astal.widget_set_class_names(self, ["Bar"])
        start_widget = Astal.Box(visible=True, hexpand=True, halign=Gtk.Align.CENTER)
        end_widget = Astal.Box(visible=True, hexpand=True, halign=Gtk.Align.CENTER)

        start_widget.set_children([Astal.Label(visible=True, label="Astal in python")])

        end_widget.set_children([Time()])

        self.add(
            Astal.CenterBox(
                visible=True,
                start_widget=start_widget,
                end_widget=end_widget,
            )
        )
