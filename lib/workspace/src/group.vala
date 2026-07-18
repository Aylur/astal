namespace AstalWorkspace {
/**
 * The set of actions that the compositor supports on a workspace group. Note that this is a bitfield enum,
 * so "has CREATE_WORKSPACE" and "no CREATE_WORKSPACE" are the two valid states for this type.
 * Note that this can change over the group's lifetime.
 */
[Flags]
public enum GroupCapabilities {
    CREATE_WORKSPACE,
}

/**
 * A group of workspaces, belonging to some set of outputs (monitors).
 * A workspace can be in multiple groups, and a group can be on multiple outputs (monitors) at once.
 * Most of the time you don't need to worry about groups directly; if you just want the workspaces on a specific
 * monitor, use a MonitorView object.
 *
 * As a ListModel, it exposes every workspace belonging to the group;
 * lists of outputs and workspaces are also available through properties.
 */
public class WorkspaceGroup : Object, ListModel {
    private WorkspaceManager manager;
    private unowned ExtWorkspaceGroupHandleV1 handle;

    private const ExtWorkspaceGroupHandleV1Listener listener = {
        handle_capabilities,
        handle_output_enter,
        handle_output_leave,
        handle_workspace_enter,
        handle_workspace_leave,
        handle_removed,
    };

    /**
     * The group's current capabilities (i.e. can you create workspaces in it).
     * Note that a group's capabilities can change over its lifetime.
     */
    public GroupCapabilities capabilities { get; private set; }
    private GroupCapabilities pending_capabilities;

    /**
     * The set of Wayland outputs (monitors) the group belongs to.
     */
    public GenericArray<AstalWl.Output> outputs { get; private set; }
    private GenericArray<AstalWl.Output> pending_added_outputs;
    private GenericArray<AstalWl.Output> pending_removed_outputs;

    /**
     * The workspaces belonging to this group.
     */
    public GenericArray<Workspace> workspaces { get; private set; }
    private GenericArray<Workspace> pending_added_workspaces;
    private GenericArray<Workspace> pending_removed_workspaces;

    // Helper for workspaces to be able to assign themselves to this handle
    internal unowned ExtWorkspaceGroupHandleV1 _get_handle() {
        return handle;
    }

    /**
     * Get a workspace at a specific position in the group's list, or null if out-of-bounds.
     */
    public Object? get_item(uint position) {
        if (position >= workspaces.length) {
            return null;
        } else {
            return workspaces[position];
        }
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
            return workspaces.length;
        }
    }

    public uint get_n_items() {
        return workspaces.length;
    }

    /**
     * Ask the compositor to create a workspace with the specified name in this group.
     * This does nothing if the group doesn't have the CREATE_WORKSPACE capability.
     */
    public void create_workspace(string name) {
        handle.create_workspace(name);
        manager._autocommit();
    }

    internal WorkspaceGroup(WorkspaceManager manager, ExtWorkspaceGroupHandleV1 handle) {
        this.manager = manager;
        this.handle = handle;
        handle.add_listener(listener, this);

        outputs = new GenericArray<AstalWl.Output>();
        pending_added_outputs = new GenericArray<AstalWl.Output>();
        pending_removed_outputs = new GenericArray<AstalWl.Output>();

        workspaces = new GenericArray<Workspace>();
        pending_added_workspaces = new GenericArray<Workspace>();
        pending_removed_workspaces = new GenericArray<Workspace>();
    }

    public override void dispose() {
        handle.destroy();
    }

    private void handle_capabilities(ExtWorkspaceGroupHandleV1 handle, ExtWorkspaceGroupHandleV1GroupCapabilities capabilities) {
        pending_capabilities = (GroupCapabilities)capabilities;
    }

    private void handle_output_enter(ExtWorkspaceGroupHandleV1 handle, Wl.Output wl_output) {
        var output = AstalWl.get_default().get_output_by_wl_output(wl_output);
        if (output == null) {
            warning("Couldn't find AstalWl.Output for output %p", wl_output);
            return;
        }
        pending_added_outputs.add(output);
    }

    private void handle_output_leave(ExtWorkspaceGroupHandleV1 handle, Wl.Output wl_output) {
        var output = AstalWl.get_default().get_output_by_wl_output(wl_output);
        if (output == null) {
            warning("Couldn't find AstalWl.Output for output %p", wl_output);
            return;
        }
        if (!pending_added_outputs.remove(output)) {
            pending_removed_outputs.add(output);
        }
    }

    private void handle_workspace_enter(ExtWorkspaceGroupHandleV1 handle, ExtWorkspaceHandleV1 workspace_handle) {
        var workspace = (Workspace)workspace_handle.get_user_data();
        pending_added_workspaces.add(workspace);
    }

    private void handle_workspace_leave(ExtWorkspaceGroupHandleV1 handle, ExtWorkspaceHandleV1 workspace_handle) {
        var workspace = (Workspace)workspace_handle.get_user_data();
        if (!pending_added_workspaces.remove(workspace)) {
            pending_removed_workspaces.add(workspace);
        }
    }

    private void handle_removed(ExtWorkspaceGroupHandleV1 handle) {
        manager.handle_group_destroy(this);
    }

    internal void apply_pending() {
        freeze_notify();

        if (_capabilities != pending_capabilities) {
            capabilities = pending_capabilities;
        }

        var outputs_changed = false;
        if (pending_added_outputs.length > 0) {
            outputs.extend(pending_added_outputs, (x) => x);
            foreach (var added in pending_added_outputs) {
                manager.group_enter_output(this, added);
            }
            pending_added_outputs = new GenericArray<AstalWl.Output>();
            outputs_changed = true;
        }
        if (pending_removed_outputs.length > 0) {
            foreach (var deleted in pending_removed_outputs) {
                outputs.remove(deleted);
                manager.group_leave_output(this, deleted);
            }
            pending_removed_outputs = new GenericArray<AstalWl.Output>();
            outputs_changed = true;
        }

        // See the manager's version of this process for why it's done like this.
        // We do not apply_pending on workspaces here because it's done in the manager.
        var workspaces_changed = false;
        var removed_count = pending_removed_workspaces.length;
        if (removed_count > 0) {
            var removed_indices = new GenericArray<uint>(removed_count);
            for (int i = 0; i < removed_count; i++) {
                uint index;
                if (workspaces.find(pending_removed_workspaces[i], out index)) {
                    removed_indices.add(index);
                } else {
                    critical("Couldn't find workspace to remove from group");
                }
            }
            removed_indices.sort((a, b) => (int)a - (int)b);

            for (int i = removed_indices.length - 1; i >= 0; i--) {
                uint run_end = removed_indices[i];
                while (i > 0 && removed_indices[i - 1] == removed_indices[i] - 1) {
                    i--;
                }
                uint run_start = removed_indices[i];
                // Items in the range [run_start, run_end] can be deleted all at once.
                uint run_length = run_end + 1 - run_start;
                workspaces.remove_range(run_start, run_length);
                items_changed(run_start, run_length, 0);
            }

            pending_removed_workspaces = new GenericArray<Workspace>();
            workspaces_changed = true;
        }
        if (pending_added_workspaces.length > 0) {
            uint before_length = workspaces.length;
            uint count = pending_added_workspaces.length;
            workspaces.extend_and_steal((owned)pending_added_workspaces);
            pending_added_workspaces = new GenericArray<Workspace>();

            items_changed(before_length, 0, count);
            workspaces_changed = true;
        }

        if (outputs_changed) {
            notify_property("outputs");
        }
        if (workspaces_changed) {
            notify_property("workspaces");
        }

        thaw_notify();
    }
}
}