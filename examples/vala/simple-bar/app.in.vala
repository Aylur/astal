class App : Astal.Application {
    public static App instance;

    public override void request (string msg, SocketConnection conn) {
        print(@"$msg\n");
        AstalIO.write_sock.begin(conn, "ok");
    }

    public override void activate () {
        foreach (var mon in this.monitors)
            add_window(new Bar(mon));

        apply_css("@STYLE@");
    }

    public static void main(string[] args) {
        var instance_name = "vala";

        App.instance = new App() {
            instance_name = instance_name
        };

        try {
            App.instance.acquire_socket();
            App.instance.run(null);
        } catch (Error err) {
            print(AstalIO.send_message(instance_name, string.joinv(" ", args)));
        }
    }
}
