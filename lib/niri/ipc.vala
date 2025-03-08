namespace AstalNiri {
/**
 * Helper class for IPC.
 */
internal class IPC {
    private SocketConnection conn;
    private DataInputStream istream;
    private DataOutputStream ostream;

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

    private IPC(SocketConnection conn) {
        this.conn = conn;
        istream = new DataInputStream(conn.input_stream);
        ostream = new DataOutputStream(conn.output_stream);
    }

    public static IPC? connect() {
        var conn = connection();
        if (conn == null)
            return null;

        return new IPC(conn);
    }

    public void send(Json.Node node) throws Error {
        ostream.put_string(Json.to_string(node, false));
        ostream.put_string("\n");
        ostream.flush();
    }

    public async void send_async(Json.Node node) throws Error {
        ostream.put_string(Json.to_string(node, false));
        ostream.put_string("\n");
        yield ostream.flush_async();
    }

    public Json.Node recv() throws Error {
        var line = istream.read_line();
        return Json.from_string(line);
    }

    public async Json.Node recv_async() throws Error {
        var line = yield istream.read_line_async();
        return Json.from_string(line);
    }

    public void close() {
        try {
            conn.close(null);
        } catch (Error err) {
            critical("%s", err.message);
        }
    }

    public async void close_async() {
        try {
            yield conn.close_async(Priority.DEFAULT, null);
        } catch (Error err) {
            critical("%s", err.message);
        }
    }
}
}
