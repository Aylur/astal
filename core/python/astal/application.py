import sys

from collections.abc import Callable
from gi.repository import Astal, Gio

RequestHandler = Callable[[str, Callable[[str], None]], None]


class _Application(Astal.Application):
    __gtype_name__ = "AstalPython"

    def __init__(self) -> None:
        super().__init__()
        self.request_handler: RequestHandler | None = None

    def request(self, msg: str, conn: Gio.SocketConnection) -> None:
        if self.request_handler:
            self.request_handler(
                msg,
                lambda response: Astal.write_sock(
                    conn,
                    str(response),
                    lambda _, res: Astal.write_sock_finish(res),
                ),
            )
        else:
            super().request(msg, conn)

    def start(
        self,
        instance_name: str | None = None,
        gtk_theme: str | None = None,
        icon_theme: str | None = None,
        cursor_theme: str | None = None,
        css: str | None = None,
        hold: bool | None = True,
        request_handler: RequestHandler | None = None,
        main: Callable | None = None,
    ) -> None:
        if request_handler:
            self.request_handler = request_handler
        if hold:
            self.hold()
        if instance_name:
            self.set_instance_name(instance_name)
        if gtk_theme:
            self.set_gtk_theme(gtk_theme)
        if icon_theme:
            self.set_icon_theme(icon_theme)
        if cursor_theme:
            self.set_cursor_theme(cursor_theme)
        if css:
            self.apply_css(css, False)
        if not self.acquire_socket():
            return print(
                f"Astal instance {self.get_instance_name()} already running",
                file=sys.stderr,
            )

        def on_activate(_):
            if main:
                main()

        self.connect("activate", on_activate)
        self.run()


App = _Application()
