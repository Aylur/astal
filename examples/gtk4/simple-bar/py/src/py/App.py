from pathlib import Path
from gi.repository import Gio, GLib, Astal, GLib, AstalIO
from py.Bar import Bar


class App(Astal.Application):
    __gtype_name__ = "App"
    instance_name = "simple-bar"

    # this is where request handlers can be implemented
    # that will be used to handle `astal` cli invocations
    def do_astal_application_request(self, request, conn):
        print("incoming request", request)
        AstalIO.write_sock(conn, "response", None)

    # this is the method that will be invoked on `app.run()`
    # this is where everything should be initialized and instantiated
    def do_activate(self):
        self.apply_css("resource:///main.css", False)
        self.add_window(Bar())

    @staticmethod
    def main(argv):
        GLib.set_prgname(App.instance_name)
        App.instance = App(instance_name=App.instance_name)

        try:
            # `app.acquire_socket()` needed for the request API to work
            App.instance.acquire_socket()

            # if it succeeds we can run the app
            return App.instance.run([])
        except Exception:
            # if it throws an error it means there is already an instance
            # with `instance_name` running, so we just send a request instead
            response = AstalIO.send_request(
                App.instance_name,
                argv.join(" "),
            )
            print(response)
            return 0
