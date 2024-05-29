from collections.abc import Callable
from gi.repository import Astal, Gio

RequestHandler = Callable[[str, Callable[[str], None]], None]


class _Application(Astal.Application):
    def __init__(self) -> None:
        super().__init__()
        self.request_handler: RequestHandler | None = None

    def do_request(self, msg: str, conn: Gio.SocketConnection) -> None:
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
            super().do_request(msg, conn)

    def start(
        self,
        instance_name: str | None = None,
        gtk_theme: str | None = None,
        icon_theme: str | None = None,
        cursor_theme: str | None = None,
        css: str | None = None,
        hold: bool | None = True,
        request_handler: RequestHandler | None = None,
        callback: Callable | None = None,
    ) -> None:
        if request_handler:
            self.request_handler = request_handler
        if hold:
            self.hold()
        if instance_name:
            self.instance_name = instance_name
        if gtk_theme:
            self.gtk_theme = gtk_theme
        if icon_theme:
            self.icon_theme = icon_theme
        if cursor_theme:
            self.cursor_theme = icon_theme
        if css:
            self.apply_css(css, False)
        if not self.acquire_socket():
            print(f"Astal instance {self.instance_name} already running")
            return

        def on_activate(app):
            if callback:
                callback()

        self.connect("activate", on_activate)
        self.run()


App = _Application()
