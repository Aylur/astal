#!/usr/bin/env python3
import gi

gi.require_version("Astal", "3.0")
gi.require_version("Gtk", "3.0")
gi.require_version("Gdk", "3.0")
gi.require_version("Gio", "2.0")
gi.require_version("GObject", "2.0")

gi.require_version("AstalBattery", "0.1")
gi.require_version("AstalWp", "0.1")
gi.require_version("AstalNetwork", "0.1")
gi.require_version("AstalTray", "0.1")
gi.require_version("AstalMpris", "0.1")
gi.require_version("AstalHyprland", "0.1")

import sys
import subprocess
from gi.repository import Gtk, Gdk, Gio, GLib
from widget.Bar import Bar
from pathlib import Path

scss = str(Path(__file__).parent.resolve() / "style.scss")
css = "/tmp/style.css"


class App(Gtk.Application):
    __gtype_name__ = "App"

    def _init_css(self):
        subprocess.run(["sass", scss, css])
        provider = Gtk.CssProvider()
        provider.load_from_path(css)

        Gtk.StyleContext.add_provider_for_screen(
            Gdk.Screen.get_default(),
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
            if len(argv) >= 3 and argv[1] == "toggle" and argv[2] == "bar":
                self.bar.set_visible(not self.bar.get_visible())
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


if __name__ == "__main__":
    app = App()
    GLib.set_prgname("simple-bar")
    app.run(sys.argv)
