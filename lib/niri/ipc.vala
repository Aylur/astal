namespace AstalNiri {
/**
 * Helper class for IPC.
 */
internal class IPC {
    private SocketConnection conn;
    private DataInputStream istream;
    private DataOutputStream ostream;

    private IPC(SocketConnection _conn) {
        conn = _conn;
        istream = new DataInputStream(conn.input_stream);
        ostream = new DataOutputStream(conn.output_stream);
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
