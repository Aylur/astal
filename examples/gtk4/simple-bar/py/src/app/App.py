from gi.repository import Gio, GLib, Gtk, Gdk, GLib
from bar.Bar import Bar


class App(Gtk.Application):
    __gtype_name__ = "App"

    def _init_css(self):
        provider = Gtk.CssProvider()
        provider.load_from_resource("/style.css")

        Gtk.StyleContext.add_provider_for_display(
            Gdk.Display.get_default(),
            provider,
            Gtk.STYLE_PROVIDER_PRIORITY_USER,
        )

    # this is the method that will be invoked on `app.run()`
    # this is where everything should be initialized and instantiated
    def do_command_line(self, command_line):
        argv = command_line.get_arguments()

        if command_line.get_is_remote():
            # app is already running we can print to remote
            command_line.print_literal("hello from the main instance\n")

            # or for example, we could toggle the visibility of the bar
            if argv.length >= 3 and argv[1] == "toggle" and argv[2] == "bar":
                self.bar.visible = not self.bar.visible
        else:
            # main instance, initialize stuff here
            self._init_css()
            self.bar = Bar()
            self.add_window(self.bar)

        return 0

    def __init__(self) -> None:
        super().__init__(
            application_id="my.awesome.simple-bar",
            flags=Gio.ApplicationFlags.HANDLES_COMMAND_LINE,
        )

    @staticmethod
    def main(argv):
        App.instance = App()
        GLib.set_prgname("simple-bar")
        return App.instance.run(argv)
