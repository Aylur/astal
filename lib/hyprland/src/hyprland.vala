namespace AstalHyprland {
public Hyprland get_default() {
    return Hyprland.get_default();
}

public class Hyprland : Object {
    private static string HIS = GLib.Environment.get_variable("HYPRLAND_INSTANCE_SIGNATURE");
    private static string RUN_DIR = GLib.Environment.get_user_runtime_dir();

    private static Hyprland _instance;
    public static Hyprland? get_default() {
        if (_instance != null) return _instance;

        var HIS = GLib.Environment.get_variable("HYPRLAND_INSTANCE_SIGNATURE");
        if (HIS == null) {
            critical("Hyprland is not running");
            return null;
        }

        var h = new Hyprland();
        _instance = h;

        h.socket2 = h.connection("socket2");
        h.watch_socket.begin(new DataInputStream(h.socket2.input_stream));
        try {
            h.init();
        } catch (Error err) {
            critical("could not initialize: %s", err.message);
            return null;
        }

        return _instance;
    }

    // monitors, workspaces, clients
    private HashTable<int, Monitor> _monitors =
        new HashTable<int, Monitor>((i) => i, ((a, b) => a == b));

    private HashTable<int, Workspace> _workspaces =
        new HashTable<int, Workspace>((i) => i, ((a, b) => a == b));

    private HashTable<string, Workspace> _workspace_addresses =
        new HashTable<string, Workspace>(str_hash, str_equal);
    private int next_workspace_handle = int.MIN;
    private uint workspace_generation = 0;

    private HashTable<string, Client> _clients =
        new HashTable<string, Client>(str_hash, str_equal);

    public List<weak Monitor> monitors { owned get { return _monitors.get_values(); } }
    public List<weak Workspace> workspaces { owned get { return _workspaces.get_values(); } }
    public List<weak Client> clients { owned get { return _clients.get_values(); } }

    public Monitor get_monitor(int id) {
        return _monitors.get(id);
    }
    public Workspace get_workspace(int id) {
        return _workspaces.get(id);
    }
    /**
     * Resolve the canonical identity returned in workspace JSON.
     */
    public Workspace? get_workspace_by_address(string address) {
        return _workspace_addresses.get(address);
    }

    internal Workspace? workspace_from_json(Json.Object? obj) {
        var identity = WorkspaceIdentity.from_json(obj);
        if (identity == null) return null;
        var ws = get_workspace_by_address(identity.address);
        if (ws != null && identity.number != null && ws.id != (int)identity.number) return null;
        return ws;
    }

    private Workspace? ensure_workspace(Json.Object? obj) {
        var identity = WorkspaceIdentity.from_json(obj);
        if (identity == null) return null;
        var existing = get_workspace_by_address(identity.address);
        if (existing != null) {
            if (identity.number != null && existing.id != (int)identity.number) return null;
            return existing;
        }

        int id;
        if (identity.number != null) {
            id = (int)identity.number;
            // Never alias an unrelated object onto an occupied compatibility ID.
            if (_workspaces.contains(id)) return null;
        } else {
            int? handle = null;
            do {
                handle = WorkspaceIdentity.take_handle(ref next_workspace_handle);
                if (handle == null) return null;
            } while (_workspaces.contains((int)handle));
            id = (int)handle;
        }
        var ws = new Workspace();
        ws.identify(id, identity.address);
        _workspaces.insert(id, ws);
        _workspace_addresses.insert(identity.address, ws);
        return ws;
    }

    private void update_workspaces(Json.Array arr, bool announce) {
        var present = new List<weak Workspace>();
        var added = new List<Workspace>();
        foreach (var node in arr.get_elements()) {
            if (node.get_node_type() != Json.NodeType.OBJECT) continue;
            var obj = node.get_object();
            var before = workspace_from_json(obj);
            var ws = ensure_workspace(obj);
            if (ws == null) continue;
            present.append(ws);
            if (before == null) added.append(ws);
        }
        // Every object exists before resolving cross-references.
        foreach (var node in arr.get_elements()) {
            if (node.get_node_type() != Json.NodeType.OBJECT) continue;
            var ws = workspace_from_json(node.get_object());
            if (ws != null) ws.sync(node.get_object());
        }
        foreach (var ws in workspaces) {
            if (present.find(ws) != null) continue;
            Workspace removed = ws;
            removed.removed();
            _workspace_addresses.remove(removed.address);
            _workspaces.remove(removed.id);
            if (announce) workspace_removed(removed.id);
        }
        if (announce) {
            foreach (var ws in added) workspace_added(ws);
            notify_property("workspaces");
        }
    }

    private async void sync_focused_workspace() throws Error {
        var value = yield message_async("j/activeworkspace");
        focused_workspace = workspace_from_json(Json.from_string(value).get_object());
    }

    public Client? get_client(string address) {
        if ((address == "") || (address == null)) return null;

        if (address.substring(0, 2) == "0x") return _clients.get(address.substring(2, -1));

        return _clients.get(address);
    }

    public Monitor? get_monitor_by_name(string name) {
        foreach (var mon in monitors) {
            if (mon.name == name) return mon;
        }
        return null;
    }

    public Workspace? get_workspace_by_name(string name) {
        foreach (var ws in workspaces) {
            if (ws.name == name) return ws;
        }
        return null;
    }

    public Workspace focused_workspace { get; private set; }
    public Monitor focused_monitor { get; private set; }
    public Client focused_client { get; private set; }

    // other props
    public List<Bind> binds {
        owned get {
            var list = new List<Bind>();
            try {
                var arr = Json.from_string(message("j/binds")).get_array();
                foreach (var b in arr.get_elements()) {
                    list.append(new Bind.from_json(b.get_object()));
                }
            } catch (Error err) {
                critical(err.message);
            }
            return list;
        }
    }

    public Position cursor_position {
        owned get {
            return new Position.cursorpos(message("cursorpos"));
        }
    }

    // signals
    public signal void event(string event, string args);

    // TODO: nag vaxry for fullscreenv2
    // public signal void fullscreen (bool fullscreen);
    public signal void minimize(Client client, bool minimize);
    public signal void floating(Client client, bool floating);
    public signal void urgent(Client client);
    public signal void client_moved(Client client, Workspace ws);

    public signal void submap(string name);
    public signal void keyboard_layout(string keyboard, string layout);
    public signal void config_reloaded();

    // state
    public signal void client_added(Client client);
    public signal void client_removed(string address);
    public signal void workspace_added(Workspace workspace);
    public signal void workspace_removed(int id);
    public signal void monitor_added(Monitor monitor);
    public signal void monitor_removed(int id);

    private SocketConnection socket2;

    private SocketConnection? connection(string socket) {
        var path = RUN_DIR + "/hypr/" + HIS + "/." + socket + ".sock";
        try {
            return new SocketClient().connect(new UnixSocketAddress(path), null);
        } catch (Error err) {
            critical(err.message);
            return null;
        }
    }

    // Preserve event ordering across asynchronous snapshot requests.
    private async void watch_socket(DataInputStream stream) {
        try {
            while (true) {
                var line = yield stream.read_line_async(Priority.DEFAULT, null);
                if (line == null) break;
                try {
                    yield handle_event(line);
                } catch (Error err) {
                    // A failed snapshot must not terminate the event reader.
                    critical(err.message);
                }
            }
        } catch (Error err) {
            critical(err.message);
        }
    }

    private void write_socket(
        string message,
        out SocketConnection conn,
        out DataInputStream stream
    ) throws Error {
        conn = connection("socket");
        if (conn != null) {
            conn.output_stream.write(message.data, null);
            stream = new DataInputStream(conn.input_stream);
        } else {
            stream = null;
            critical("could not write to the Hyprland socket");
        }
    }

    public string message(string message) {
        SocketConnection? conn;
        DataInputStream? stream;
        try {
            write_socket(message, out conn, out stream);
            if ((stream != null) && (conn != null)) {
                var res = stream.read_upto("\x04", -1, null, null);
                conn.close(null);
                return res;
            }
        } catch (Error err) {
            critical(err.message);
        }
        return "";
    }

    public async string message_async(string message) {
        SocketConnection? conn;
        DataInputStream? stream;
        try {
            write_socket(message, out conn, out stream);
            if ((stream != null) && (conn != null)) {
                var res = yield stream.read_upto_async("\x04", -1, Priority.DEFAULT, null, null);
                conn.close(null);
                return res;
            }
        } catch (Error err) {
            critical(err.message);
        }
        return "";
    }

    public void dispatch(string dispatcher, string args) {
        var msg = "dispatch " + dispatcher + " " + args;
        message_async.begin(msg, (_, res) => {
                var err = message_async.end(res);
                if (err != "ok") critical("dispatch error: %s", err);
            });
    }

    public void move_cursor(int x, int y) {
        dispatch("movecursor", x.to_string() + " " + y.to_string());
    }

    // TODO: nag vaxry to make socket events and hyprctl more consistent
    private void init() throws Error {
        var mons = Json.from_string(message("j/monitors")).get_array();
        var wrkspcs = Json.from_string(message("j/workspaces")).get_array();
        var clnts = Json.from_string(message("j/clients")).get_array();

        // create
        foreach (var mon in mons.get_elements()) {
            var id = (int)mon.get_object().get_member("id").get_int();
            var m = new Monitor();
            _monitors.insert(id, m);

            if (mon.get_object().get_member("focused").get_boolean()) focused_monitor = m;
        }
        foreach (var ws in wrkspcs.get_elements()) {
            if (ws.get_node_type() == Json.NodeType.OBJECT) ensure_workspace(ws.get_object());
        }
        foreach (var clnt in clnts.get_elements()) {
            var addr = clnt.get_object().get_member("address").get_string();
            _clients.set(addr.replace("0x", ""), new Client());
        }

        // init
        foreach (var c in clnts.get_elements()) {
            var addr = c.get_object().get_member("address").get_string();
            get_client(addr).sync(c.get_object());
        }
        update_workspaces(wrkspcs, false);
        foreach (var mon in mons.get_elements()) {
            var id = (int)mon.get_object().get_member("id").get_int();
            get_monitor(id).sync(mon.get_object());
        }

        // focused
        focused_workspace = workspace_from_json(Json.from_string(message("j/activeworkspace")).get_object());

        var active_window = Json.from_string(message("j/activewindow")).get_object();
        focused_client = get_client(WorkspaceIdentity.string_member(active_window, "address") ?? "");
    }

    ~Hyprland() {
        if (socket2 != null) {
            try {
                socket2.close(null);
            } catch (Error err) {
                critical(err.message);
            }
        }
    }

    public async void sync_monitors() throws Error {
        var str = yield message_async("j/monitors");
        var arr = Json.from_string(str).get_array();
        foreach (var obj in arr.get_elements()) {
            var id = (int)obj.get_object().get_int_member("id");
            var m = get_monitor(id);
            if (m != null) m.sync(obj.get_object());
        }
    }

    public async void sync_workspaces() throws Error {
        var generation = workspace_generation;
        var str = yield message_async("j/workspaces");
        if (generation != workspace_generation) return;
        var arr = Json.from_string(str).get_array();
        update_workspaces(arr, true);
    }

    public async void sync_clients() throws Error {
        var str = yield message_async("j/clients");
        var arr = Json.from_string(str).get_array();
        foreach (var obj in arr.get_elements()) {
            var addr = obj.get_object().get_string_member("address");
            var c = get_client(addr);
            if (c != null) c.sync(obj.get_object());
        }
        foreach (var ws in workspaces) ws.refresh_clients();
    }

    private async bool try_add_client(string addr) throws Error {
        if ((addr == "") || (get_client(addr) != null)) {
            return true;
        }

        var client = new Client();
        _clients.insert(addr, client);
        yield sync_workspaces();
        yield sync_clients();
        client_added(client);
        notify_property("clients");
        return false;
    }

    private async void handle_event(string line) throws Error {
        var args = line.split(">>", 2);
        if (args.length != 2) return;

        switch (args[0]) {
            case "workspacev2": {
                yield sync_workspaces();
                yield sync_monitors();
                yield sync_focused_workspace();
                break;
            }
            case "focusedmon": {
                var argv = args[1].split(",", 2);
                yield sync_monitors();
                focused_monitor = get_monitor_by_name(argv[0]);
                yield sync_focused_workspace();
                break;
            }
            case "fullscreen": {
                yield sync_clients();
                break;
            }
            case "monitorremoved": {
                var id = get_monitor_by_name(args[1]).id;
                _monitors.get(id).removed();
                _monitors.remove(id);
                monitor_removed(id);
                notify_property("monitors");
                break;
            }
            case "monitoraddedv2": {
                var id = int.parse(args[1].split(",", 2)[0]);
                var mon = new Monitor();
                _monitors.insert(id, mon);
                yield sync_monitors();
                monitor_added(mon);
                notify_property("monitors");
                break;
            }
            // Both supported protocols emit v2; v1 may contain only a display
            // name and is not an independent lifetime transition.
            case "createworkspace":
            case "destroyworkspace":
                break;
            case "destroyworkspacev2": {
                workspace_generation++;
                var address = args[1].split(",", 2)[0];
                var ws = get_workspace_by_address(address);
                if (ws != null) {
                    ws.removed();
                    _workspace_addresses.remove(address);
                    _workspaces.remove(ws.id);
                    workspace_removed(ws.id);
                    notify_property("workspaces");
                }
                // A current snapshot may already contain the next lifetime.
                yield sync_workspaces();
                yield sync_clients();
                yield sync_monitors();
                yield sync_focused_workspace();
                break;
            }
            case "createworkspacev2":
            case "moveworkspacev2":
            case "renameworkspace":
            case "activespecial": {
                yield sync_workspaces();
                yield sync_clients();
                yield sync_monitors();
                yield sync_focused_workspace();
                break;
            }
            case "activelayout": {
                var argv = args[1].split(",");
                keyboard_layout(argv[0], argv[1]);
                break;
            }
            // first event that signals a new client when it opens as an active window
            case "activewindowv2": {
                yield try_add_client(args[1]);
                focused_client = get_client(args[1]);
                break;
            }
            case "openwindow": {
                var addr = args[1].split(",")[0];
                if (yield try_add_client(addr)) {
                    yield sync_workspaces();
                    yield sync_clients();
                }
                break;
            }
            case "closewindow": {
                _clients.get(args[1]).removed();
                _clients.remove(args[1]);
                yield sync_workspaces();
                client_removed(args[1]);
                notify_property("clients");
                break;
            }
            case "movewindowv2": {
                yield sync_workspaces();
                yield sync_clients();
                var argv = args[1].split(",");
                var client = get_client(argv[0]);
                if (client != null && client.workspace != null) {
                    client_moved(client, client.workspace);
                    client.moved_to(client.workspace);
                }
                break;
            }
            case "submap": {
                submap(args[1]);
                break;
            }
            case "changefloatingmode": {
                var argv = args[1].split(",");
                yield sync_clients();
                floating(get_client(argv[0]), argv[1] == "0");
                break;
            }
            case "urgent": {
                urgent(get_client(args[1]));
                break;
            }
            case "minimize": {
                var argv = args[1].split(",");
                yield sync_clients();
                minimize(get_client(argv[0]), argv[1] == "0");
                break;
            }
            case "windowtitlev2": {
                yield sync_clients();
                break;
            }
            // TODO:
            case "togglegroup":
            case "moveintogroup":
            case "moveoutofgroup":
            case "ignoregrouplock":
            case "lockgroups": {
                break;
            }
            case "configreloaded": {
                config_reloaded();
                break;
            }
            default: break;
        }

        event(args[0], args[1]);
    }
}
}
