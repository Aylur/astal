#!/usr/bin/env python3
import versions
from gi.repository import Astal, Gio
from widget.Bar import Bar
from pathlib import Path

css = str(Path(__file__).parent.resolve() / "style.css")


class App(Astal.Application):
    def __init__(self):
        super().__init__()
        self.acquire_socket()
        self.run(None)

    def do_request(self, msg: str, conn: Gio.SocketConnection) -> None:
        print(msg)
        Astal.write_sock(conn, "hello")

    def do_activate(self) -> None:
        self.hold()
        self.apply_css(css, True)
        for mon in self.get_monitors():
            self.add_window(Bar(mon))


App()
