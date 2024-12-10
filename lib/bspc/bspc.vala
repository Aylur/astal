namespace AstalBspc {
    public Bspc get_default() {
        return Bspc.get_default();
    }

    private uint8[] fmt_msg(string s) {
        return (s.replace(" ", "\x00") + "\x00").data;
    }


    public class Bspc : Object {
        private static string SOCKET_PATH = "/tmp/bspwm_0_0-socket";
        private static Bspc _instance;
        private SocketConnection socket;
        

        public Desktop active_desktop { get; private set; }
        public Node active_node { get; private set; }

        private HashTable<string, Desktop> _desktops = new HashTable<string, Desktop>(str_hash, str_equal);
        private HashTable<string, Node> _nodes   = new HashTable<string, Node>(str_hash, str_equal);

        public List<weak Desktop> desktops { owned get { return _desktops.get_values(); } }
        public List<weak Node> nodes { owned get { return _nodes.get_values(); } }


        public static Bspc? get_default() {
            if (_instance != null)
                return _instance;

            var SOCKET_PATH = "/tmp/bspwm_0_0-socket";

            if (!GLib.FileUtils.test(SOCKET_PATH, GLib.FileTest.EXISTS)) {
                critical("BSPWM is not running");
                return null;
            }

            var i = new Bspc();

            _instance = i;

            i.socket = i.new_socket();

            i.socket.output_stream.write_async.begin(fmt_msg("subscribe all"), GLib.Priority.DEFAULT, null, (_, task) => {
                i.socket.output_stream.write_async.end(task);
                i.watch_socket(new DataInputStream(i.socket.input_stream));
            });

            print(
                i.message("query -T -n")
            );


            return i;
        }


        public Node? get_node(string id) {
            if ( id == "" || id == null)
                return null;
            return _nodes.get(id);
        }

        public Desktop? get_desktop  (string id) { 
            if ( id == "" || id == null)
                return null;

            return _desktops.get(id);
        }

        private SocketConnection? new_socket() throws Error {
            return new SocketClient().connect(new UnixSocketAddress(SOCKET_PATH), null);
        }

        private async SocketConnection? new_socket_async() throws Error {
            return yield new SocketClient().connect_async(new UnixSocketAddress(SOCKET_PATH), null);
        }


        private void watch_socket(DataInputStream stream) {
            stream.read_line_async.begin(GLib.Priority.DEFAULT, null, (_, res) => {
                    try {
                        var line = stream.read_line_async.end(res);

                        handle_event.begin(line, (_, res) => handle_event.end(res) );

                        watch_socket(stream);
                    } catch (Error e) {
                        critical(e.message);
                    }
            });
        }


        public void write_socket(string message, out SocketConnection conn, out DataInputStream data_stream) throws Error {
            conn = new_socket();

            if (conn != null) {
                conn.output_stream.write(fmt_msg(message), null);
                data_stream = new DataInputStream(conn.input_stream);
            } else {
                data_stream = null;
                critical("could not write to the Bspwm socket");
            }
        }

        public async void write_socket_async(string message, out SocketConnection conn, out DataInputStream data_stream) throws Error {
            conn = yield new_socket_async();
            yield conn.output_stream.write_async(fmt_msg(message), GLib.Priority.DEFAULT, null);
        
            data_stream = new DataInputStream(conn.input_stream);
        }

        public string message(string message) {
            SocketConnection? conn;
            DataInputStream? data_stream;

            try {
                write_socket(message, out conn, out data_stream);

                if (data_stream != null && conn != null) {
                    string res = data_stream.read_upto("\x04", -1, null, null);
                    conn.close(null);

                    return res;
                }
            } catch (GLib.Error e) {
                critical(e.message);
            }

            return "";
        }


        public async string message_async(string message) {
            SocketConnection? conn;
            DataInputStream? data_stream;

            try {
                //yield write_socket_async(message, out conn, out data_stream);
                write_socket(message, out conn, out data_stream);

                if (data_stream != null && conn != null) {
                    //string res = yield data_stream.read_upto_async("\x04", -1, GLib.Priority.DEFAULT, null, null);
                    string res = data_stream.read_upto("\x04", -1, null, null);
                    conn.close();

                    return res;
                }

            } catch (Error e) {
                critical(e.message);
            }
            return "";
        }


        ~Bspc() {
            if (socket != null) {
                try {
                    socket.close();
                } catch (GLib.Error e) {
                    critical(e.message);
                }
            }
        }


        private async void handle_event(string line) {
            var args = line.split(" ");
            print(line);

            switch (args[0]) {
                case "node_add":
                    break;
                case "node_remove":
                    break;
                case "node_focus":
                    break;
                case "desktop_focus":
                    break;
                default:
                    break;
            }
        }
    }
}
