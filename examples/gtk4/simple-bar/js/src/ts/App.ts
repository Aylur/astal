import GObject from "gi://GObject"
import Astal from "gi://Astal?version=4.0"
import Gio from "gi://Gio"
import GLib from "gi://GLib"
import AstalIO from "gi://AstalIO"
import Bar from "./Bar"

export default class App extends Astal.Application {
    static {
        GObject.registerClass(this)
    }

    static instance: App
    static instanceName = "simple-bar"

    // this is where request handlers can be implemented
    // that will be used to handle `astal` cli invocations
    vfunc_request(request: string, conn: Gio.SocketConnection): void {
        print("incoming request", request)
        AstalIO.write_sock(conn, "response", null)
    }

    // this is the method that will be invoked on `app.runAsync()`
    // this is where everything should be initialized and instantiated
    vfunc_activate(): void {
        this.apply_css("resource:///main.css", false)
        this.add_window(new Bar())
    }

    // entry point of our app
    static async main(argv: string[]): Promise<number> {
        GLib.set_prgname(App.instanceName)
        App.instance = new App({ instanceName: App.instanceName })

        try {
            // `app.acquire_socket()` needed for the request API to work
            App.instance.acquire_socket()

            // if it succeeds we can run the app
            return await App.instance.runAsync([])
        } catch (error) {
            // if it throws an error it means there is already an instance
            // with `instanceName` running, so we just send a request instead
            const response = AstalIO.send_request(
                App.instanceName,
                argv.join(" "),
            )
            print(response)
            return 0
        }
    }
}
