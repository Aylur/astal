#!/usr/bin/env python3
import sys
import versions
import subprocess
from gi.repository import AstalIO, Astal, Gio
from widget.Bar import Bar
from pathlib import Path

scss = str(Path(__file__).parent.resolve() / "style.scss")
css = "/tmp/style.css"


class App(Astal.Application):
    def do_astal_application_request(
        self, msg: str, conn: Gio.SocketConnection
    ) -> None:
        print(msg)
        AstalIO.write_sock(conn, "hello")

    def do_activate(self) -> None:
        self.hold()
        subprocess.run(["sass", scss, css])
        self.apply_css(css, True)
        for mon in self.get_monitors():
            self.add_window(Bar(mon))


instance_name = "python"
app = App(instance_name=instance_name)

if __name__ == "__main__":
    try:
        app.acquire_socket()
        app.run(None)
    except Exception as e:
        print(AstalIO.send_message(instance_name, "".join(sys.argv[1:])))
