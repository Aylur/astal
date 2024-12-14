namespace AstalBspc {
    public Bspc get_default() {
        return Bspc.get_default();
    }

    private uint8[] fmt_msg(string s) { // "query -T -n" -> {'q', 'u', 'e', 'r', 'y', 0, '-', 'T', 0, '-', 'n', 0}
        uint8[] result = new uint8[s.length + 1]; 

        for (int i = 0; i < s.length; i++) {
            result[i] = (s[i] == ' ') ? (uint8) 0x00 : (uint8) s[i];
        }

        result[s.length] = (uint8) 0x00;

        return result;
    }

    public class Bspc : Object {
        private static string SOCKET_PATH = "/tmp/bspwm_0_0-socket";
        private static Bspc _instance;
        private SocketConnection socket;

        public Desktop? active_desktop { get; private set; }
        public Node? active_node { get; private set; }

        private HashTable<string, Desktop> _desktops = new HashTable<string, Desktop>(str_hash, str_equal);
        private HashTable<string, Node> _nodes   = new HashTable<string, Node>(str_hash, str_equal);

        public List<weak Desktop> desktops { owned get { return _desktops.get_values(); } }
        public List<weak Node> nodes { owned get { return _nodes.get_values(); } }

        public static Bspc? get_default() {
            if (_instance != null)
                return _instance;

            string SOCKET_PATH = "/tmp/bspwm_0_0-socket";

            if (!GLib.FileUtils.test(SOCKET_PATH, GLib.FileTest.EXISTS)) {
                critical("BSPWM is not running");
                return null;
            }

            var i = new Bspc();

            _instance = i;
            i.socket = i.new_socket();

            uint8[] subscribe_all = { 
                's', 'u', 'b', 's', 'c', 'r', 'i', 'b', 'e', 0, 
                'a', 'l', 'l', 0 
            };

            try {
                i.socket.output_stream.write(subscribe_all, null);
                i.watch_socket(new DataInputStream(i.socket.input_stream));
            } catch (Error e) {
                critical(e.message);
                return null;
            }

            try {
                i.init();
            } catch (Error e) {
                critical("could not initialize: %s", e.message);
                return null;
            }

            return i;
        }
        
        // define signals uwu

       public signal void event(string event, string args);



        public signal void node_added(Node node);
        public signal void node_removed(string id);
        public signal void node_moved(Node node, Desktop desktop);
        public signal void node_swapped(Node node1, Node node2);

        // ToDo
        //public signal void desktop_added(Desktop desktop);
        //public signal void desktop_removed(string id);

        //public signal void monitor_added(Desktop desktop);
        //public signal void monitor_removed(string id);

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

        private SocketConnection? new_socket() {
            try {
                return new SocketClient().connect(new UnixSocketAddress(SOCKET_PATH), null);
            } catch (Error e) {
                critical(e.message);
                return null;
            }
        }

        private async SocketConnection? new_socket_async() {
            try {
                return yield new SocketClient().connect_async(new UnixSocketAddress(SOCKET_PATH), null);
            } catch (Error e) {
                critical(e.message);
                return null;
            }
        }


        private void watch_socket(DataInputStream stream) {
            stream.read_line_async.begin(GLib.Priority.DEFAULT, null, (_, res) => {
                try {
                    var line = stream.read_line_async.end(res);

                    handle_event.begin(line, (_, task) => {
                        try {
                            handle_event.end(task);
                        } catch (Error e) {
                            critical(e.message);
                        }
                        watch_socket(stream);
                    });

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
                yield write_socket_async(message, out conn, out data_stream);

                if (data_stream != null && conn != null) {
                    string res = yield data_stream.read_upto_async("\x04", -1, GLib.Priority.DEFAULT, null, null);
                    conn.close();

                    return res;
                }

            } catch (Error e) {
                critical(e.message);
            }
            return "";
        }


        private void init() throws Error {
            foreach (var desktop_id in message("query -D").split("\n")) {
                if (desktop_id.length > 1) {
                    var str = message("query -T -d " + desktop_id);
                    var desktop_data = Json.from_string(str);
                    if (desktop_data != null) {
                        Desktop desktop = new Desktop();
                        desktop.id = desktop_id;
                        _desktops.insert(desktop.id, desktop);
                   }
                }
            }

            foreach (var desktop in desktops) {
                foreach (var node_id in message("query -N -d " + desktop.id).split("\n")) {
                    if (node_id.length > 1) {
                        var node_data = Json.from_string(message("query -T -n " + node_id));
                        if (node_data != null && node_data.get_object().get_member("client") != null) {
                            var node = new Node();
                            node.id = node_id;
                            node.desktop = desktop;
                            _nodes.insert(node.id, node);
                        }
                    }
                }
            }

            sync_nodes.begin((_, task) => {
                try {
                    sync_nodes.end(task);
                } catch (Error e) {
                    critical(e.message);
                }
            });

            sync_desktops.begin((_, task) => {
                try {
                    sync_desktops.end(task);
                } catch (Error e) {
                    critical(e.message);
                }
            });

        }


        ~Bspc() {
            if (socket != null) {
                try {
                    socket.close();
                } catch (GLib.Error e) {
                    critical(e.message);
                } finally {
                    socket = null;
                }
            }
        }


        private async void sync_nodes() throws Error {
            foreach (var node in nodes) {
                var str = yield message_async("query -T -n " + node.id);
                node.sync(Json.from_string(str).get_object());
            }
            notify_property("nodes");
        }

        private async void sync_desktops() throws Error {
            foreach (var desk in desktops) {
                var str = yield message_async("query -T -d " + desk.id);
                desk.sync(Json.from_string(str).get_object());
            }
            notify_property("desktops");
        }



        private async void handle_event(string line) throws Error  {
            var args = line.split(" ");

            switch (args[0]) {
                case "node_add":
                    var n = new Node();
                    n.id = args[4];
                    n.desktop = get_desktop(args[2]);

                    _nodes.insert(n.id, n);
                    node_added(n);
                    yield sync_nodes();
                    yield sync_desktops();


                    break;

                case "node_remove":
                    _nodes.remove(args[3]);
                    node_removed(args[3]);
                    yield sync_desktops();
                    yield sync_nodes();
                    break;

                case "node_focus":
                    if (active_node != null) {
                        active_node.focused = false;
                        active_node.notify_property("focused");
                    }

                    active_node = get_node(args[3]);
                    active_node.focused = true;
                    active_node.notify_property("focused");

                    notify_property("active-node");
                    break;

                case "node_geometry":
                    Node n = get_node(args[3]);
                    string? input = args[4];



                    MatchInfo? match_info;
                    Regex regex = new Regex("^(\\d+)x(\\d+)\\+(\\d+)\\+(\\d+)$");

                    if (regex.match(input, 0, out match_info)) {
                        int width = int.parse(match_info.fetch(1));
                        int height = int.parse(match_info.fetch(2));
                        int x = int.parse(match_info.fetch(3));
                        int y = int.parse(match_info.fetch(4));

                        n.width = width;
                        n.height = height;
                        n.x = x;
                        n.y = y;
                        n.notify_property("width");
                        n.notify_property("height");
                        n.notify_property("x");
                        n.notify_property("y");
                    }
                    break;

                case "node_state":
                    var n = get_node(args[3]);

                    if (n != null) {
                        n.floating = (args[4] == "floating" && args[5] == "on") 
                            || (args[4] == "tiled" && args[5] == "off");
                        n.notify_property("floating");
                    }
                    break;

                case "node_stack":
                    break;

                case "node_transfer":
                    var origin = get_desktop(args[2]);
                    var target = get_desktop(args[5]);
                    var n = get_node(args[3]);

                    if ( n != null && origin != null && origin != null ) {
                        n.desktop = target;
                        n.notify_property("desktop");

                        origin._nodes.remove(n.id);
                        origin.notify_property("nodes");

                        target._nodes.insert(n.id, n);
                        target.notify_property("nodes");
                    }

                    break;

                case "node_swap":
                    var origin = get_desktop(args[2]);
                    var target = get_desktop(args[5]);
                    var n1 = get_node(args[3]);
                    var n2 = get_node(args[6]);

                    if ( n1 != null && origin != null && origin != null ) {
                        n1.desktop = target;
                        n1.notify_property("desktop");
                        n2.desktop = origin;
                        n2.notify_property("desktop");

                        origin._nodes.remove(n1.id);
                        origin._nodes.insert(n2.id, n2);
                        origin.notify_property("nodes");

                        target._nodes.insert(n1.id, n1);
                        target._nodes.remove(n2.id);
                        target.notify_property("nodes");
                    }
                    break;
                case "pointer_action":
                    break;
                case "desktop_focus":
                    active_desktop = get_desktop(args[2]);
                    break;
                default:
                    ReportFormat data = new ReportFormat(line.split(":"));

                    if (data.T == null) {
                        if (active_node != null) {
                            active_node.focused = false;
                            active_node.notify_property("focused");
                        }

                        active_node = null;
                        notify_property("active-node");
                    }
                    break;
            }
        }
    }
}
