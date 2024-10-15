namespace AstalIO {
public errordomain AppError {
    NAME_OCCUPIED,
    TAKEOVER_FAILED,
}

public interface Application : Object {
    public abstract void quit() throws Error;
    public abstract void inspector() throws Error;
    public abstract void toggle_window(string window) throws Error;

    public abstract string instance_name { owned get; construct set; }
    public abstract void acquire_socket() throws Error;
    public virtual void request(string msg, SocketConnection conn) throws Error {
        write_sock.begin(conn, @"missing response implementation on $instance_name");
    }
}

public SocketService acquire_socket(Application app, out string sock) throws Error {
    var name = app.instance_name;
    foreach (var instance in get_instances()) {
        if (instance == name) {
            throw new AppError.NAME_OCCUPIED(@"$name is occupied");
        }
    }

    var rundir = Environment.get_user_runtime_dir();
    var dir = @"$rundir/astal";
    var path = @"$dir/$name.sock";
    sock = path;

    if (!FileUtils.test(dir, FileTest.IS_DIR)) {
        File.new_for_path(path).make_directory_with_parents(null);
    }

    if (FileUtils.test(path, FileTest.EXISTS)) {
        try {
            File.new_for_path(path).delete(null);
        } catch (Error err) {
            throw new AppError.TAKEOVER_FAILED("could not delete previous socket");
        }
    }

    var service = new SocketService();
    service.add_address(
        new UnixSocketAddress(path),
        SocketType.STREAM,
        SocketProtocol.DEFAULT,
        null,
        null
    );

    service.incoming.connect((conn) => {
        read_sock.begin(conn, (_, res) => {
            try {
                string message = read_sock.end(res);
                app.request(message != null ? message.strip() : "", conn);
            } catch (Error err) {
                critical(err.message);
            }
        });
        return false;
    });

    return service;
}

public static List<string> get_instances() {
    var list = new List<string>();
    var prefix = "io.Astal.";

    try {
        DBusImpl dbus = Bus.get_proxy_sync(
            BusType.SESSION,
            "org.freedesktop.DBus",
            "/org/freedesktop/DBus"
        );

        foreach (var busname in dbus.list_names()) {
            if (busname.has_prefix(prefix))
                list.append(busname.replace(prefix, ""));
        }
    } catch (Error err) {
        critical(err.message);
    }

    return list;
}

public static void quit_instance(string instance) {
    try {
        IApplication proxy = Bus.get_proxy_sync(
            BusType.SESSION,
            "io.Astal." + instance,
            "/io/Astal/Application"
        );

        proxy.quit();
    } catch (Error err) {
        critical(err.message);
    }
}

public static void open_inspector(string instance) {
    try {
        IApplication proxy = Bus.get_proxy_sync(
            BusType.SESSION,
            "io.Astal." + instance,
            "/io/Astal/Application"
        );

        proxy.inspector();
    } catch (Error err) {
        critical(err.message);
    }
}

public static void toggle_window_by_name(string instance, string window) {
    try {
        IApplication proxy = Bus.get_proxy_sync(
            BusType.SESSION,
            "io.Astal." + instance,
            "/io/Astal/Application"
        );

        proxy.toggle_window(window);
    } catch (Error err) {
        critical(err.message);
    }
}

public static string send_message(string instance_name, string msg) {
    var rundir = Environment.get_user_runtime_dir();
    var socket_path = @"$rundir/astal/$instance_name.sock";
    var client = new SocketClient();

    try {
        var conn = client.connect(new UnixSocketAddress(socket_path), null);
        conn.output_stream.write(msg.concat("\x04").data);

        var stream = new DataInputStream(conn.input_stream);
        return stream.read_upto("\x04", -1, null, null);
    } catch (Error err) {
        printerr(err.message);
        return "";
    }
}

public async string read_sock(SocketConnection conn) throws IOError {
    var stream = new DataInputStream(conn.input_stream);
    return yield stream.read_upto_async("\x04", -1, Priority.DEFAULT, null, null);
}

public async void write_sock(SocketConnection conn, string response) throws IOError  {
    yield conn.output_stream.write_async(response.concat("\x04").data, Priority.DEFAULT);
}

[DBus (name="io.Astal.Application")]
private interface IApplication : DBusProxy {
    public abstract void quit() throws GLib.Error;
    public abstract void inspector() throws GLib.Error;
    public abstract void toggle_window(string window) throws GLib.Error;
}

[DBus (name="org.freedesktop.DBus")]
private interface DBusImpl : DBusProxy {
    public abstract string[] list_names() throws Error;
}
}
