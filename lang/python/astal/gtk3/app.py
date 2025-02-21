import sys

from typing import Callable, Optional, Any, Dict

from gi.repository import Astal, AstalIO, Gio

class AstalPy(Astal.Application):
    request_handler: Optional[Callable[[str, Callable[[Any], None]], None]] = None

    def do_astal_application_request(self, msg: str, conn: Gio.SocketConnection):
        if callable(self.request_handler):
            def respond(response: Any):
                AstalIO.write_sock(conn, str(response), None, None)
            self.request_handler(msg, respond)
        else:
            AstalIO.Application.do_request(self, msg, conn)

    def quit(self, code: int = 0):
        super().quit()
        sys.exit(code)

    def apply_css(self, css: str, reset: bool = False):
        super().apply_css(css, reset)

    def start(self, **config: Any):
        config.setdefault("client", lambda *_: (print(f'Astal instance "{self.get_instance_name()}" is already running'), sys.exit(1)))
        config.setdefault("hold", True)
        
        self.request_handler = config.get("request_handler")
        
        if "css" in config:
            self.apply_css(config["css"])
        if "icons" in config:
            self.add_icons(config["icons"])
        
        for key in ["instance_name", "gtk_theme", "icon_theme", "cursor_theme"]:
            if key in config:
                self.set_property(key, config[key])
        
        def on_activate(_):
            if callable(config.get("main")):
                config["main"]()
            if config["hold"]:
                self.hold()
        
        self.connect("activate", on_activate)

        try:
            self.acquire_socket()

        except Exception:
            return config["client"](lambda msg: AstalIO.send_message(self.get_instance_name(), msg), *sys.argv[1:])
        
        self.run()
        
        return self

App = AstalPy()
