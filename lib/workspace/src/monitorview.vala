namespace AstalWorkspace {
/**
 * A helper proxy object to list all of the workspaces on a specific monitor (Wayland output)
 * The workspaces are only accessible via ListModel; the relevant groups can be accessed via property.
 */
public class WorkspaceMonitorView : Object, ListModel {
    private AstalWl.Output output;

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

    internal WorkspaceMonitorView(WorkspaceManager manager, AstalWl.Output output) {
        this.output = output;

        // Typically there will be one group per monitor, so we can reserve the space up front.
        groups = new GenericArray<WorkspaceGroup>(1);
        group_connections = new GenericArray<ulong>(1);
        manager.group_enter_output.connect(handle_group_enter_output);
        manager.group_leave_output.connect(handle_group_leave_output);

        foreach (var group in manager.groups) {
            foreach (var group_output in group.outputs) {
                handle_group_enter_output(group, group_output);
            }
        }
    }

    public override void dispose() {
        // Explicitly disconnect the lambdas here, so that their capture structs are freed
        var count = groups.length;
        for (int i = 0; i < count; i++) {
            groups[i].disconnect(group_connections[i]);
        }
        groups.remove_range(0, count);
        group_connections.remove_range(0, count);

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
}
}