namespace AstalNiri {
public Niri get_default() {
    return Niri.get_default();
}

public class Niri : Object {
    public signal void event(Json.Node event);

    static Niri _instance;

    private static SocketConnection? connection() {
        var socket_path = Environment.get_variable("NIRI_SOCKET");
        if (socket_path == null) {
            critical("Niri is not running");
            return null;
        }

        try {
            return new SocketClient().connect(new UnixSocketAddress(socket_path), null);
        } catch (Error err) {
            critical("%s", err.message);
            return null;
        }
    }

    public static Niri? get_default() {
        if (_instance != null)
            return _instance;

        var conn = connection();
        if (conn == null)
            return null;

        _instance = new Niri();
        _instance.watch_socket.begin(conn);

        return _instance;
    }

    private async void watch_socket(SocketConnection conn) {
        var istream = new DataInputStream(conn.input_stream);
        var ostream = new DataOutputStream(conn.output_stream);

        try {
            ostream.put_string("\"EventStream\"\n");
            ostream.flush();

            var line = yield istream.read_line_async();
            if (line != "{\"Ok\":\"Handled\"}") {
                critical("Failed to start event stream: %s", line);
                return;
            }

            while ((line = yield istream.read_line_async()) != null) {
                var ev = Json.from_string(line);
                if (ev == null) {
                    event.emit(ev);
                }
            }
        } catch (Error e) {
            critical("%s", e.message);
            return;
        }
    }

    private Json.Node? message(Json.Node message) {
        var conn = connection();
        if (conn == null)
            return null;

        var istream = new DataInputStream(conn.input_stream);
        var ostream = new DataOutputStream(conn.output_stream);

        try {
            ostream.put_string(Json.to_string(message, false));
            ostream.flush();

            var line = istream.read_line();
            if (line == null) {
                critical("Niri did not respond");
                return null;
            }

            return Json.from_string(line);
        } catch (Error err) {
            critical("%s", err.message);
            return null;
        } finally {
            try {
                conn.close(null);
            } catch (Error err) {
                critical("%s", err.message);
            }
        }
    }

    private async Json.Node? message_async(Json.Node message) {
        var conn = connection();
        if (conn == null)
            return null;

        var istream = new DataInputStream(conn.input_stream);
        var ostream = new DataOutputStream(conn.output_stream);

        try {
            ostream.put_string(Json.to_string(message, false));
            yield ostream.flush_async();

            var line = yield istream.read_line_async();
            if (line == null) {
                critical("Niri did not respond");
                return null;
            }

            return Json.from_string(line);
        } catch (Error err) {
            critical("%s", err.message);
            return null;
        } finally {
            try {
                yield conn.close_async(Priority.DEFAULT, null);
            } catch (Error err) {
                critical("%s", err.message);
            }
        }
    }
}
}
