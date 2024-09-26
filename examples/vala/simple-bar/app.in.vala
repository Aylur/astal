class App : Astal.Application {
    public static App instance;

    public override void request (string msg, SocketConnection conn) {
        print(@"$msg\n");
        Astal.write_sock.begin(conn, "hello");
    }

    public override void activate () {
        foreach (var mon in this.monitors)
            add_window(new Bar(mon));

        apply_css("@STYLE@");
    }

    public static void main(string[] args) {
        var instance_name = "simple-bar";

        App.instance = new App() {
            instance_name = instance_name
        };

        if (App.instance.acquire_socket()) {
            App.instance.run(null);
        } else {
            print(Astal.Application.send_message(instance_name, string.joinv(" ", args)));
        }
    }
}
