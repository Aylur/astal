namespace AstalNiri {
public Niri get_default() {
    return Niri.get_default();
}

public class Niri : Object {
    [CCode(has_target = false)]
    private delegate void EventHandler(Niri self, Json.Object object);

    private HashTable<uint64?, Workspace> _workspaces =
        new HashTable<uint64?, Workspace>(int64_hash, int64_equal);

    private HashTable<uint64?, Window> _windows =
        new HashTable<uint64?, Window>(int64_hash, int64_equal);

    private static HashTable<string, EventHandler> event_handlers =
        new HashTable<string, EventHandler>(str_hash, str_equal);

    /**
     * Array of keyboard layouts.
     */
    string[] keyboard_layouts { get; private set; }

    /**
     * Index of currently active keyboard layout.
     */
    uint8 current_keyboard_layout { get; private set; }

    /**
     * Emitted when an event has been received. Contains the raw JSON received.
     */
    public signal void event(Json.Node event);

    /**
     * Emitted when the list of workspaces changes.
     */
    public signal void workspaces_changed();

    /**
     * Emitted when the list of windows changes.
     */
    public signal void windows_changed();

    /**
     * Emitted when a new window has been opened.
     */
    public signal void window_opened(Window window);

    /**
     * Emitted when a window has been focused.
     */
    public signal void window_focus_changed(Window? window);

    static Niri _instance;

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

    private static int sort_workspaces(Workspace a, Workspace b) {
        var a_output = a.output;
        var b_output = b.output;

        if (a_output == b_output) {
            var a_idx = a.idx;
            var b_idx = b.idx;
            return (int) (a_idx > b_idx) - (int) (a_idx < b_idx);
        }

        return strcmp(a_output, b_output);
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
        if (ipc == null)
            return null;

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
        if (ipc == null)
            return null;

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
        var workspaces = event.get_array_member("workspaces");

        _workspaces.remove_all();
        foreach (var element in workspaces.get_elements()) {
            var workspace = new Workspace.from_json(element.get_object());
            _workspaces.insert(workspace.id, workspace);
        }
    }

    private void on_workspace_activated(Json.Object event) {
        var id = event.get_int_member("id");
        var focused = event.get_boolean_member("focused");

        var activated_workspace = get_workspace(id);
        if (activated_workspace == null) {
            unknown_workspace(id);
            return;
        }

        var output = activated_workspace.output;
        foreach (var workspace in _workspaces.get_values()) {
            if (workspace.output != output)
                continue;

            var activated = workspace == activated_workspace;
            workspace.is_active = activated;
            if (focused)
                workspace.is_focused = activated;
        }

        activated_workspace.activated.emit();
    }

    private void on_workspace_active_window_changed(Json.Object event) {
        var workspace_id = event.get_int_member("workspace_id");
        var active_window_id = event.get_member("active_window_id");

        var workspace = get_workspace(workspace_id);
        if (workspace == null) {
            unknown_workspace(workspace_id);
            return;
        }

        if (active_window_id.is_null()) {
            workspace.active_window_id = null;
        } else {
            workspace.active_window_id = active_window_id.get_int();
        }

        workspace.active_window_changed.emit();
    }

    private void on_windows_changed(Json.Object event) {
        var windows = event.get_array_member("windows");

        _windows.remove_all();
        foreach (var element in windows.get_elements()) {
            var window = new Window.from_json(element.get_object());
            _windows.insert(window.id, window);
        }

        windows_changed.emit();
    }

    private void on_window_opened_or_changed(Json.Object event) {
        var window_object = event.get_object_member("window");
        var window_id = window_object.get_int_member("id");

        var window = get_window(window_id);
        if (window != null) {
            window.sync(window_object);

            window.changed.emit();
        } else {
            window = new Window.from_json(window_object);
            _windows.insert(window_id, window);

            window_opened.emit(window);
        }

        if (window.is_focused) {
            foreach (var win in get_windows()) {
                win.is_focused = win.id == window_id;
            }
        }
    }

    private void on_window_closed(Json.Object event) {
        var id = event.get_int_member("id");

        var window = _windows.take(id);
        if (window == null) {
            unknown_window(id);
            return;
        }

        window.closed.emit();
    }

    private void on_window_focus_changed(Json.Object event) {
        var focused = event.get_member("id");

        if (focused.is_null()) {
            foreach (var window in get_windows()) {
                window.is_focused = false;
            }

            window_focus_changed.emit(null);
        } else {
            var id = focused.get_int();
            foreach (var window in get_windows()) {
                window.is_focused = window.id == id;
            }

            var window = get_window(id);
            if (window == null) {
                unknown_window(id);
                return;
            }
            window_focus_changed.emit(window);
        }
    }

    private void on_keyboard_layouts_changed(Json.Object event) {
        var layouts = event.get_object_member("keyboard_layouts");
        var names = layouts.get_array_member("names");

        var builder = new StrvBuilder();
        foreach (var element in names.get_elements()) {
            builder.add(element.get_string());
        }
        keyboard_layouts = builder.end();

        current_keyboard_layout = (uint8) layouts.get_int_member("current_idx");
    }

    private void on_keyboard_layout_switched(Json.Object event) {
        current_keyboard_layout = (uint8) event.get_int_member("idx");
    }

    public List<unowned Workspace> get_workspaces() {
        var res = _workspaces.get_values().copy();
        res.sort(sort_workspaces);
        return res;
    }

    public List<unowned Window> get_windows() {
        return _windows.get_values();
    }

    public unowned Window? get_window(uint64 id) {
        return _windows.get(id);
    }

    public unowned Workspace? get_workspace(uint64 id) {
        return _workspaces.get(id);
    }
}
}
