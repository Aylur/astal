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

        public Desktop? focused_desktop { get; private set; }
        public Node? focused_node { get; private set; }

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
        public signal void desktop_added(Desktop desktop);
        public signal void desktop_removed(string id);

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
                    var res = data_stream.read_upto("\x04", -1, null, null);
                    conn.close(null);

                    if (res != null && res.length > 1)
                        res = res.substring(0, res.length - 1); // remove last \n
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
                    var res = yield data_stream.read_upto_async("\x04", -1, GLib.Priority.DEFAULT, null, null);
                    conn.close();

                    if (res != null && res.length > 1) 
                        res = res.substring(0, res.length - 1);

                    return res;
                }

            } catch (Error e) {
                critical(e.message);
            }
            return "";
        }


        private void init() throws Error {
            foreach (var desktop_id in message("query -D").split("\n")) {
                Desktop desktop = new Desktop();
                desktop.id = desktop_id;
                _desktops.insert(desktop.id, desktop);
            }

            foreach (var desktop in desktops) {
                var nodes = message("query -N -d " + desktop.id + " -n .leaf").split("\n");

                foreach (var node_id in nodes) {
                    if (node_id.length > 1) {
                        var node = new Node();
                        node.id = node_id;
                        node.desktop = desktop;
                        desktop._nodes.set(node.id, node);
                        _nodes.insert(node.id, node);
                    }
                }
            }

            foreach (var desktop in desktops) {
                var str = message("query -T -d " + desktop.id);
                desktop.sync(Json.from_string(str).get_object());
            }

            foreach (var node in nodes) {
                var str = message("query -T -n " + node.id);
                node.sync(Json.from_string(str).get_object());
            }

            focused_desktop = get_desktop(message("query -D -d focused"));
            focused_node = get_node(message("query -N -n focused"));
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
        }

        private async void handle_event(string line) throws Error  {
            var args = line.split(" ");

            switch (args[0]) {
                // node events
                case "node_add":
                    var n = new Node();
                    n.id = args[4];
                    n.desktop = get_desktop(args[2]);
                    n.desktop._nodes.insert(n.id, n);
                    n.desktop.notify_property("nodes");

                    _nodes.insert(n.id, n);
                    node_added(n);
                    yield sync_nodes();
                    yield sync_desktops();
                    break;

                case "node_remove":
                    _desktops.get(args[2])._nodes.remove(args[3]);
                    _desktops.get(args[2]).notify_property("nodes");
                    _nodes.get(args[3]).removed();
                    _nodes.remove(args[3]);
                    node_removed(args[3]);
                    yield sync_desktops();
                    yield sync_nodes();
                    break;

                case "node_swap":
                    var origin = get_desktop(args[2]);
                    var target = get_desktop(args[5]);
                    var n1 = get_node(args[3]);
                    var n2 = get_node(args[6]);

                    if ( n1 != null && origin != null && origin != null ) {
                        n1.desktop = target;
                        n1.moved_to(n1.desktop);
                        n2.desktop = origin;
                        n2.moved_to(n2.desktop);

                        origin._nodes.remove(n1.id);
                        origin._nodes.insert(n2.id, n2);
                        origin.notify_property("nodes");

                        target._nodes.insert(n1.id, n1);
                        target._nodes.remove(n2.id);
                        target.notify_property("nodes");
                    }
                    break;

                case "node_transfer":
                    var origin = get_desktop(args[2]);
                    var target = get_desktop(args[5]);
                    var n = get_node(args[3]);

                    if ( n != null && origin != null && origin != null ) {
                        n.desktop = target;

                        origin._nodes.remove(n.id);
                        origin.notify_property("nodes");

                        target._nodes.insert(n.id, n);
                        target.notify_property("nodes");
                    }
                    break;

                case "node_focus":
                    focused_node = get_node(args[3]);
                    break;

                case "node_activate":
                    break;

                case "node_presel":
                    break;

                case "node_stack":
                    break;

                case "node_geometry":
                    Node n = get_node(args[3]);
                    string? input = args[4];

                    MatchInfo? match_info;
                    Regex regex = new Regex("^(\\d+)x(\\d+)\\+(\\d+)\\+(\\d+)$");

                    if (regex.match(input, 0, out match_info)) {
                        n.width = int.parse(match_info.fetch(1));
                        n.height = int.parse(match_info.fetch(2));
                        n.x = int.parse(match_info.fetch(3));
                        n.y = int.parse(match_info.fetch(4));
                    }
                    break;

                case "node_state":
                    var n = get_node(args[3]);

                    if (n != null && args[5] == "on") {
                        n.state = NodeState.parse(args[4]);
                    }
                    break;

                case "node_flag":
                    break;

                case "node_layer":
                    break;

                // desktop events
                case "desktop_add":
                    Desktop d = new Desktop();
                    d.id = args[2];
                    d.name = args[3];
                    _desktops.insert(d.id, d);
                    desktop_added(d);
                    break;

                case "desktop_rename":
                    var d = get_desktop(args[2]);
                    d.name = args[4];
                    break;

                case "desktop_remove":
                    _desktops.get(args[2]).removed();
                    _desktops.remove(args[2]);
                    desktop_removed(args[2]);
                    break;

                case "desktop_swap":
                    break;

                case "desktop_transfer":
                    break;

                case "desktop_focus":
                    focused_desktop = get_desktop(args[2]);
                    break;

                case "desktop_activate":
                    break;

                case "desktop_layout":
                    break;

                // other
                case "pointer_action":
                    break;

                default:
                    ReportFormat data = new ReportFormat(line.split(":"));

                    if (data.T == null) {
                        focused_node = null;
                    }
                    break;
            }
        }
    }
}
