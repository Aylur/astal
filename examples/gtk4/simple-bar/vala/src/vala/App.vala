class App : Astal.Application {
    static App instance;

    // this is where request handlers can be implemented
    // that will be used to handle `astal` cli invocations
    public override void request(string request, GLib.SocketConnection conn) {
        print(@"incoming request: $request\n");

        AstalIO.write_sock.begin(conn, "response", null);
    }

    // this is the method that will be invoked on `app.run()`
    // this is where everything should be initialized and instantiated
    public override void activate() {
        this.apply_css("resource:///main.css", false);
        this.add_window(new Bar());
    }

    // entry point of our app
    static int main(string[] argv) {
        App.instance = new App() { instance_name = "simple-bar" };

        try {
            // `app.acquire_socket()` needed for the request API to work
            App.instance.acquire_socket();

            // if it succeeds we can run the app
            return App.instance.run(null);
        } catch (Error _) {
            // if it throws an error it means there is already an instance
            // with `instance_name` running, so we just send a request instead
            try {
                var response = AstalIO.send_request(
                    "simple-bar",
                    string.joinv(" ", argv[1:])
                );
                print(@"$response\n");
                return 0;
            } catch (Error err) {
                printerr(err.message);
                return 1;
            }
        }
    }
}
