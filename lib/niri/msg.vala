namespace AstalNiri {
public class msg : Object {
    public static string? send(string message) {
        IPC ipc;

        try {
            ipc = IPC.connect();
            if (ipc == null ) return null;
            var istream = ipc.send(Json.from_string(message));
            var line = istream.read_line();
            return line;
        } catch (Error err) {
            critical("command Error: %s", err.message);
            return err.message;
        } finally {
            ipc.close();
        }
    }

    public static async string send_async(string message) {
        IPC ipc;
        try {
            ipc = IPC.connect();
            if (ipc == null ) return "no ipc";
            var istream = ipc.send(Json.from_string(message));
            var line = yield istream.read_line_async();
            critical("%s", line);
            return line;
        } catch (Error err) {
            critical("command Error: %s", err.message);
            return err.message;
        } finally {
            ipc.close();
        }
    }


    // 
    // Simple Actions
    // 
    /** Formats a simple action for Niri's IPC   */
    private static bool send_act(string str) {
        var cmd = "{\"Action\":{\"%s\":{}}}\n".printf(str);
        var res = send(cmd);
        critical(res);
        if (res == "{\"Ok\":\"Handled\"}") return true;
        return false;
    }

}
}
