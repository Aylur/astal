namespace AstalNiri {
/**
 * Helper class for IPC.
 */
internal class IPC {
    private SocketConnection conn;
    private DataInputStream istream;
    private DataOutputStream ostream;
    public signal void event_stream(string event_type, Json.Node payload);

    private IPC(SocketConnection _conn) {
        conn = _conn;
        istream = new DataInputStream(conn.input_stream);
        ostream = new DataOutputStream(conn.output_stream);
    }

    public async void stream() {
        try {
            var istream = this.send_str("\"EventStream\"\n");
            var line = yield istream.read_line_async();
            if (line != "{\"Ok\":\"Handled\"}") {
                critical("Event Stream Error: %s", line);
                return;
            }
            line = null;
            while (true) {
                var ev = Json.from_string(yield istream.read_line_async());
                var obj = ev.get_object();
                if (obj == null || obj.get_size() != 1U) {
                    critical("Invalid event '%s'", Json.to_string(ev, false));
                    continue;
                }

                this.event_stream.emit(obj.get_members().data, ev);
            }
        } catch (Error err) {
            critical("%s", err.message);
            return;
        } finally {
            this.close();
        }
    }

    public static IPC? connect() {
        string NIRI_SOCKET = Environment.get_variable("NIRI_SOCKET");
        if (NIRI_SOCKET == null) critical("Niri is not running");

        SocketConnection conn;
        try {
            conn = new SocketClient().connect(new UnixSocketAddress(NIRI_SOCKET), null);
            if (conn == null) return null;
            return new IPC(conn);
        } catch (Error err) {
            critical("%s", err.message);
        }
        return null;
    }

    public DataInputStream send(Json.Node msg) throws Error {
        ostream.put_string(Json.to_string(msg, false));
        ostream.put_string("\n");
        ostream.flush();

        return istream;
    }
    public DataInputStream send_str(string msg) throws Error {
        ostream.put_string(msg);
        ostream.flush();

        return istream;
    }

    public void close() {
        try {
            conn.close(null);
        } catch (Error err) {
            critical("%s", err.message);
        }
    }
}
}
