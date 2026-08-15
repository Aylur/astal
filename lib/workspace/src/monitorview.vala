namespace AstalWorkspace {
/**
 * A helper proxy object to list all of the workspaces on a specific monitor (Wayland output)
 * The workspaces are only accessible via ListModel; the relevant groups can be accessed via property.
 */
public class WorkspaceMonitorView : Object, ListModel {
    private WorkspaceManager manager;
    private AstalWl.Output? output;
    /**
     * Whether the Wayland output this object was created with still exists, and thus the object will still receive updates normally.
     */
    public bool valid { get; private set; default = true; }
    /**
     * Emitted when the Wayland output this object was created with gets destroyed.
     */
    public signal void invalidate();

    private void _invalidate() {
#if !NO_GTK
        if (monitor != null) {
            if (monitor_connect_invalidate != 0) {
                monitor.disconnect(monitor_connect_invalidate);
                monitor_connect_invalidate = 0;
            }
            if (monitor_connect_connector != 0) {
                monitor.disconnect(monitor_connect_connector);
                monitor_connect_connector = 0;
            }
            monitor = null;
        }
        if (manager_connect_add_output != 0) {
            manager.disconnect(manager_connect_add_output);
            manager_connect_add_output = 0;
        }
#endif

        if (valid) {
            valid = false;
            invalidate();
        }
    }

#if !NO_GTK
    private Gdk.Monitor? monitor;
    private ulong monitor_connect_invalidate;
    private ulong monitor_connect_connector;
    private ulong manager_connect_add_output;
#endif

    private GenericArray<ulong> group_connections;
    /**
     * The groups that belong to the monitor view's monitor (Wayland output).
     */
    public GenericArray<WorkspaceGroup> groups { get; private set; }

    /**
     * Get the workspace at a specific position in the monitor view's list, or null if out-of-bounds.
     */
    public Object? get_item(uint position) {
        for (int i = 0; i < groups.length; i++) {
            var ws_list = groups[i].workspaces;
            if (position < ws_list.length) {
                return ws_list[position];
            } else {
                position -= ws_list.length;
            }
        }
        return null;
    }

    public Type item_type {
        get {
            return typeof (Workspace);
        }
    }

    public Type get_item_type() {
        return typeof (Workspace);
    }

    public uint n_items {
        get {
            return get_n_items();
        }
    }

    public uint get_n_items() {
        uint total = 0;
        foreach (var group in groups) {
            total += group.workspaces.length;
        }
        return total;
    }

    internal void finish_init() {
        manager.remove_output.connect(handle_output_remove);

        manager.group_enter_output.connect(handle_group_enter_output);
        manager.group_leave_output.connect(handle_group_leave_output);

        foreach (var group in manager.groups) {
            foreach (var group_output in group.outputs) {
                handle_group_enter_output(group, group_output);
            }
        }
    }

    internal WorkspaceMonitorView(WorkspaceManager manager, AstalWl.Output output) {
        // Most of the time these arrays will have one element, so we can reserve that space
        groups = new GenericArray<WorkspaceGroup>(1);
        group_connections = new GenericArray<ulong>(1);
        this.manager = manager;
        this.output = output;
        finish_init();
    }

#if !NO_GTK
    internal WorkspaceMonitorView.with_gdkmonitor(WorkspaceManager manager, Gdk.Monitor monitor) {
        groups = new GenericArray<WorkspaceGroup>(1);
        group_connections = new GenericArray<ulong>(1);
        // Due to how Wayland works, everything here is very asynchronous, so this function is essentially split
        this.manager = manager;
        this.monitor = monitor;
        if (monitor.valid) {
            monitor_connect_invalidate = monitor.invalidate.connect(() => _invalidate());
            if (monitor.connector != null) {
                debug("with_gdkmonitor: immediate continue for monitor %p", monitor);
                _with_gdkmonitor2();
            } else {
                monitor_connect_connector = monitor.notify["connector"].connect(() => _with_gdkmonitor2());
            }
        } else {
            _invalidate();
        }
    }

    private void _with_gdkmonitor2() {
        if (monitor_connect_connector != 0) {
            monitor.disconnect(monitor_connect_connector);
        }

        var registry = AstalWl.get_default();
        var output = registry.get_output_by_name(monitor.connector);
        if (output != null) {
            debug("with_gdkmonitor2: immediate finish for monitor %p", monitor);
            this.output = output;
            finish_init();
        } else {
            manager_connect_add_output = manager.add_named_output.connect(_with_gdkmonitor3);
        }
    }

    private void _with_gdkmonitor3(AstalWl.Output output) {
        if (output.name == monitor.connector) {
            manager.disconnect(manager_connect_add_output);
            manager_connect_add_output = 0;
            this.output = output;
            finish_init();
        }
    }

#endif

    public override void dispose() {
        // Explicitly disconnect the lambdas here, so that their capture structs are freed
        var count = groups.length;
        for (int i = 0; i < count; i++) {
            groups[i].disconnect(group_connections[i]);
        }
        groups.remove_range(0, count);
        group_connections.remove_range(0, count);

        // This function will clean up any signal connections related to acquiring the output.
        // Setting valid = false beforehand will stop it from emitting the invalidate signal.
        _valid = false;
        _invalidate();

        base.dispose();
    }

    private void handle_items_changed(WorkspaceGroup group, uint position, uint removed, uint added) {
        uint index;
        if (!groups.find(group, out index)) {
            critical("Group should've been attached to MonitorView but wasn't");
            return;
        }

        uint total_before = 0;
        for (int i = 0; i < index; i++) {
            total_before += groups[i].workspaces.length;
        }

        items_changed(total_before + position, removed, added);
    }

    private void handle_group_enter_output(WorkspaceGroup group, AstalWl.Output output) {
        if (output != this.output) {
            return;
        }
        var id = group.items_changed.connect((pos, rem, add) => handle_items_changed(group, pos, rem, add));
        group_connections.add(id);
        var before_length = get_n_items();
        groups.add(group);

        if (group.workspaces.length > 0) {
            items_changed(before_length, 0, group.workspaces.length);
        }
        notify_property("groups");
    }

    private void handle_group_leave_output(WorkspaceGroup group, AstalWl.Output output) {
        if (output != this.output) {
            return;
        }
        uint index;
        if (!groups.find(group, out index)) {
            critical("Group should've been attached to MonitorView but wasn't");
            return;
        }

        uint total_before = 0;
        for (int i = 0; i < index; i++) {
            total_before += groups[i].workspaces.length;
        }
        uint removed_count = group.workspaces.length;

        groups[index].disconnect(group_connections[index]);
        group_connections.remove_index(index);
        groups.remove_index(index);

        items_changed(total_before, removed_count, 0);
        notify_property("groups");
    }

    private void handle_output_remove(AstalWl.Output output) {
        if (output == this.output) {
            _invalidate();
        }
    }
}
}
