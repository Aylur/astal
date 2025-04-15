namespace AstalNiri {
public Niri get_default() {
    return Niri.get_default();
}
public class Niri : Object {
    [CCode(has_target = false)]
    private delegate void EventHandler(Niri self, Json.Object object);
    private static HashTable<string, EventHandler> event_handlers =
        new HashTable<string, EventHandler>(str_hash, str_equal);

    private HashTable<int64?, Workspace> _workspaces =
        new HashTable<int64?, Workspace>(int64_hash, int64_equal);
    private HashTable<int64?, Window> _windows =
        new HashTable<int64?, Window>(int64_hash, int64_equal);
    private HashTable<string?, Output> _outputs =
        new HashTable<string?, Output>(str_hash, str_equal);

    string[] keyboard_layouts { get; private set; }

    public uint8 keyboard_layout_idx { get; private set; }
    // representing Optional uint64 as -1 due to: `warning: Type `uint64?' can not be used for a GLib.Object property`. 
    // Will overflow if niri ever uses full uint64 values or someone opens 9 quintillion windows
    public int64 focused_workspace_id { get; private set; }
    public int64 focused_window_id { get; private set; }
    public string focused_output_name { get; private set; }

    public Workspace? focused_workspace { get; private set; }
    public Window? focused_window { get; private set; }
    public Output? focused_output { get; private set; }

    public List<weak Window> windows { owned get { return _windows.get_values().copy(); } }
    public List<weak Output> outputs { owned get { return _outputs.get_values().copy(); } }
    public List<weak Workspace> workspaces { owned get {
        var res = _workspaces.get_values().copy();
        res.sort(sort_workspaces);
        return res;
    } }
    //TODO: public Output? focused_output {get; private set; }

    /** An event has been received. */
    public signal void event(Json.Node event);
    /** The list of workspaces changed. */
    public signal void workspaces_changed(List<weak Workspace> workspaces);
    public signal void workspace_activated(int64 workspace, bool focused);
    public signal void workspace_active_window_changed(int64 workspace, int64 window_id);
    /** The list of windows changed. */
    public signal void windows_changed(List<weak Window> windows);
    public signal void window_opened_or_changed(Window window);
    /** A new window has been opened. */
    public signal void window_opened(Window window);
    /** An existing window has changed. */
    public signal void window_changed(Window window);
    /** A window has closed. */
    public signal void window_closed(int64 id);
    /** A window has been focused. */
    public signal void window_focus_changed(int64 window_id);

    public signal void keyboard_layouts_changed(string[] keyboard_layouts);
    public signal void keyboard_layout_switched(uint8 idx);

    static Niri _instance;

    // https://yalter.github.io/niri/niri_ipc/enum.Event.html
    static construct {
        event_handlers.insert("WorkspacesChanged",            (EventHandler) on_workspaces_changed);
        event_handlers.insert("WorkspaceActivated",           (EventHandler) on_workspace_activated);
        event_handlers.insert("WorkspaceActiveWindowChanged", (EventHandler) on_workspace_active_window_changed);
        event_handlers.insert("WindowsChanged",               (EventHandler) on_windows_changed);
        event_handlers.insert("WindowOpenedOrChanged",        (EventHandler) on_window_opened_or_changed);
        event_handlers.insert("WindowClosed",                 (EventHandler) on_window_closed);
        event_handlers.insert("WindowFocusChanged",           (EventHandler) on_window_focus_changed);
        event_handlers.insert("KeyboardLayoutsChanged",       (EventHandler) on_keyboard_layouts_changed);
        event_handlers.insert("KeyboardLayoutSwitched",       (EventHandler) on_keyboard_layout_switched);
    }

    private Niri(IPC ipc) {
        watch_socket.begin(ipc);
    }

    public static Niri? get_default() {
        if (_instance != null)
            return _instance;

        var ipc = IPC.connect();
        if (ipc == null)
            return null;

        _instance = new Niri(ipc);
        return _instance;
    }

    private static void unknown_workspace(uint64 id) {
        critical("Workspace %" + uint64.FORMAT + " not found.", id);
    }

    private static void unknown_window(uint64 id) {
        critical("Window %" + uint64.FORMAT + " not found.", id);
    }

    // this makes a single sorted list of workspaces
    // future version should return unsorted workspaces
    // and include an output class with sorted workspaces
    private static int sort_workspaces(Workspace a, Workspace b) {
        if (a.output != b.output) return strcmp(a.output, b.output);
        return (int) (a.idx > b.idx) - (int) (a.idx < b.idx);
    }

    private async void watch_socket(IPC ipc) {
        try {
            var event_stream = new Json.Node.alloc();
            event_stream.init_string("EventStream");
            yield ipc.send_async(event_stream);
            event_stream = null;

            var ev = yield ipc.recv_async();
            var line = Json.to_string(ev, false);
            if (line != "{\"Ok\":\"Handled\"}") {
                critical("%s", line);
                return;
            }
            line = null;

            while (true) {
                ev = yield ipc.recv_async();
                event.emit(ev);

                var obj = ev.get_object();
                if (obj == null || obj.get_size() != 1U) {
                    critical("Invalid event '%s'", Json.to_string(ev, false));
                    continue;
                }

                var event_type = obj.get_members().data;
                var event = obj.get_object_member(event_type);
                var handler = event_handlers.get(event_type);
                if (handler == null) {
                    warning("Unhandled event %s", event_type);
                    continue;
                }

                handler(this, event);
            }
        } catch (Error err) {
            critical("%s", err.message);
            return;
        } finally {
            yield ipc.close_async();
        }
    }

    private Json.Node? message(Json.Node message) {
        var ipc = IPC.connect();
        if (ipc == null) return null;

        try {
            ipc.send(message);
            return ipc.recv();
        } catch (Error err) {
            critical("%s", err.message);
            return null;
        } finally {
            ipc.close();
        }
    }

    private async Json.Node? message_async(Json.Node message) {
        var ipc = IPC.connect();
        if (ipc == null) return null;

        try {
            yield ipc.send_async(message);
            return yield ipc.recv_async();
        } catch (Error err) {
            critical("%s", err.message);
            return null;
        } finally {
            yield ipc.close_async();
        }
    }

    private void on_workspaces_changed(Json.Object event) {
        var workspaces_arr = event.get_array_member("workspaces");

        _outputs.remove_all();
        _workspaces.remove_all();
        foreach (var element in workspaces_arr.get_elements()) {
            var workspace = new Workspace.from_json(element.get_object());
            _workspaces.insert(workspace.id, workspace);
            // if (_outputs.get(workspace.output) == null) {
                // requeres additional message to retrieve output data
                // _outputs.insert(workspace.output, )
            // }
            if(workspace.is_focused) {
                update_focused_workspace(workspace.id);
            }
        }
        workspaces_changed(workspaces);
        notify_property("workspaces");
    }

    private void on_workspace_activated(Json.Object event) {
        var id = event.get_int_member("id");
        var focused = event.get_boolean_member("focused");

        var activated_workspace = get_workspace(id);
        if (activated_workspace == null) {
            unknown_workspace(id);
            return;
        }

        // could get the activated workspace faster by tracking outputs
        var output = activated_workspace.output;
        foreach (var workspace in _workspaces.get_values()) {
            if (workspace.output != output) continue;

            bool activated = workspace == activated_workspace;
            workspace.is_active = activated;
        }
        if (focused) update_focused_workspace(id);

        workspace_activated(id, focused);
        activated_workspace.activated();
    }

    private void on_workspace_active_window_changed(Json.Object event) {
        var workspace_id = event.get_int_member("workspace_id");
        var _active_window_id = event.get_member("active_window_id");

        var workspace = get_workspace(workspace_id);
        if (workspace == null) {
            unknown_workspace(workspace_id);
            return;
        }

        if (_active_window_id.is_null()) {
            workspace.active_window_id = -1;
            workspace_active_window_changed(workspace_id, -1);
        } else {
            var active_window_id = _active_window_id.get_int();
            workspace.active_window_id = active_window_id;
            workspace_active_window_changed(workspace_id, active_window_id);
        }
            workspace.active_window_changed(workspace.active_window_id);
    }

    private void on_windows_changed(Json.Object event) {
        var windows_arr = event.get_array_member("windows");

        _windows.remove_all();
        foreach (var element in windows_arr.get_elements()) {
            var window = new Window.from_json(element.get_object());
            _windows.insert(window.id, window);
            if (window.is_focused) update_focused_window(window.id);
        }
        windows_changed(windows);
        notify_property("windows");
    }

    private void on_window_opened_or_changed(Json.Object event) {
        var window_object = event.get_object_member("window");
        var window_id = window_object.get_int_member("id");

        var window = _windows.get(window_id);
        if (window != null) {
            window.sync(window_object);
            window.changed();
            window_changed(window);
        } else {
            window = new Window.from_json(window_object);
            _windows.insert(window_id, window);
            window_opened(window);
            notify_property("windows");
        }

        if (window.is_focused) {
            update_focused_window(window.id);
        }
        window_opened_or_changed(window);
    }

    private void on_window_closed(Json.Object event) {
        var id = event.get_int_member("id");

        var window = _windows.take(id);
        if (window == null) {
            unknown_window(id);
            return;
        }

        window_closed(id);
        window.closed();
        notify_property("windows");
    }

    private void on_window_focus_changed(Json.Object event) {
        var _id = event.get_member("id");
        int64 id = -1;
        if (!_id.is_null())  id = _id.get_int();
        
        update_focused_window(id);
    }

    private void on_keyboard_layouts_changed(Json.Object event) {
        var layouts = event.get_object_member("keyboard_layouts");
        var names = layouts.get_array_member("names");

        var builder = new StrvBuilder();
        foreach (var element in names.get_elements()) {
            builder.add(element.get_string());
        }
        keyboard_layouts = builder.end();

        keyboard_layout_idx = (uint8) layouts.get_int_member("current_idx");
        keyboard_layouts_changed(keyboard_layouts);
    }

    private void on_keyboard_layout_switched(Json.Object event) {
        keyboard_layout_idx = (uint8) event.get_int_member("idx");

        keyboard_layout_switched(keyboard_layout_idx);
    }

    public unowned Window? get_window(int64 id) {
        return _windows.get(id);
    }

    public unowned Workspace? get_workspace(int64 id) {
        return _workspaces.get(id);
    }
    private unowned void update_focused_workspace(int64? _id) {
        int64 id = -1;
        if(_id != null) id = _id;
        if (focused_workspace_id == -1) {
            focused_workspace_id = id;
            return;
        }

        var prev = _workspaces.get(focused_workspace_id);
        if(prev != null) prev.is_focused = focused_workspace_id == id;

        focused_workspace_id = id;
        var new_focused = _workspaces.get(focused_workspace_id);
        new_focused.is_focused = true;
        focused_workspace = new_focused;
    }
    private void update_focused_window(int64? _id) {
        int64 id = -1;
        if(_id != null) id = _id;

        // remove focused state from previous window
        var prev = _windows.get(focused_window_id);
        if (prev != null) {
            prev.is_focused = focused_window_id == id;
            if (prev.id == id) {
                notify_property("focused_window");
                return;
            }
        }

        focused_window_id = id;
        var new_focused = _windows.get(focused_window_id);
        if (new_focused != null) {
            new_focused.is_focused = true;
            focused_window = new_focused;
        } else {
          focused_window = null;
        }

        window_focus_changed(focused_window_id);
    }
}
}
