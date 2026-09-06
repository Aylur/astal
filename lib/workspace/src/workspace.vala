namespace AstalWorkspace {
/**
 * The set of possible states that a workspace can be in. Note that this is a bitfield enum,
 * so any combination of these is allowed. Not every compositor will use all the states.
 */
[Flags]
public enum WorkspaceState {
    ACTIVE,
    URGENT,
    HIDDEN,
}

/**
 * The set of actions that the compositor supports on a workspace. Note that this is a bitfield enum,
 * so any combination of these is allowed. Note that this can change over the workspace's lifetime
 * (e.g. ACTIVATE is supported only when inactive, and vice versa)
 */
[Flags]
public enum WorkspaceCapabilities {
    ACTIVATE,
    DEACTIVATE,
    REMOVE,
    ASSIGN,
}

public class Workspace : Object {
    private WorkspaceManager manager;
    private unowned ExtWorkspaceHandleV1 handle;

    private const ExtWorkspaceHandleV1Listener listener = {
        handle_id,
        handle_name,
        handle_coordinates,
        handle_state,
        handle_capabilities,
        handle_removed,
    };

    /**
     * The workspace's ID. This is not required to be set by the compositor,
     * but if it is set, it will not change during the workspace's lifetime,
     * and will be unique during its lifetime.
     * IDs are not human-readable and shouldn't be displayed,
     * they're for identifying workspaces across sessions.
     */
    public string? id { get; private set; default = null; }
    private string? pending_id = null;
    /**
     * The workspace's human-readable name. It is not guaranteed to be unique,
     * and can change during the workspace's lifetime.
     */
    public string name { get; private set; }
    private string? pending_name = null;
    /**
     * The workspace's coordinates.
     * Compositors which arrange workspaces geometrically will use this property
     * to describe where the workspace is in space;
     * other compositors may use the workspace's number or leave it as null.
     */
    public GenericArray<uint32>? coordinates { get; private set; default = null; }
    private GenericArray<uint32>? pending_coordinates;
    /**
     * The workspace's current state.
     */
    public WorkspaceState state { get; private set; }
    private WorkspaceState pending_state;
    /**
     * The workspace's current capabilities. These directly correspond to methods on this object.
     * Note that a workspace's capabilities can change during its lifetime.
     */
    public WorkspaceCapabilities capabilities { get; private set; }
    private WorkspaceCapabilities pending_capabilities;

    // Helper for the manager to be able to call group.handle_workspace_leave.
    internal unowned ExtWorkspaceHandleV1 _get_handle() {
        return handle;
    }

    /**
     * Ask the compositor to activate the workspace.
     * This does nothing if the workspace doesn't have the ACTIVATE capability.
     */
    public void activate() {
        handle.activate();
        manager._autocommit();
    }

    /**
     * Ask the compositor to deactivate the workspace.
     * This does nothing if the workspace doesn't have the DEACTIVATE capability.
     */
    public void deactivate() {
        handle.deactivate();
        manager._autocommit();
    }

    /**
     * Ask the compositor to assign the workspace to a group.
     * This does nothing if the workspace doesn't have the ASSIGN capability.
     */
    public void assign_to_group(WorkspaceGroup group) {
        handle.assign(group._get_handle());
        manager._autocommit();
    }

    /**
     * Ask the compositor to remove the workspace.
     * This does nothing if the workspace doesn't have the REMOVE capability.
     */
    public void remove() {
        handle.remove();
        manager._autocommit();
    }

    internal Workspace(WorkspaceManager manager, ExtWorkspaceHandleV1 handle) {
        this.manager = manager;
        this.handle = handle;
        // Used by groups to recover these objects from the handles.
        handle.set_user_data(this);
        handle.add_listener(listener, this);
    }

    public override void dispose() {
        debug("workspace %p dispose", this);
        handle.destroy();
        base.dispose();
    }

    private void handle_id(ExtWorkspaceHandleV1 handle, string id) {
        pending_id = id;
    }

    private void handle_name(ExtWorkspaceHandleV1 handle, string name) {
        pending_name = name;
    }

    private void handle_coordinates(ExtWorkspaceHandleV1 handle, Wl.Array coordinates) {
        uint32* coords_data = coordinates.data;
        var count = coordinates.size / sizeof (uint32);
        pending_coordinates = new GenericArray<uint32>((uint)count);
        for (var i = 0; i < count; i++) {
            pending_coordinates.add(coords_data[i]);
        }
    }

    private void handle_state(ExtWorkspaceHandleV1 handle, ExtWorkspaceHandleV1State state) {
        pending_state = (WorkspaceState)state;
    }

    private void handle_capabilities(ExtWorkspaceHandleV1 handle, ExtWorkspaceHandleV1WorkspaceCapabilities capabilities) {
        pending_capabilities = (WorkspaceCapabilities)capabilities;
    }

    private void handle_removed(ExtWorkspaceHandleV1 handle) {
        manager.handle_workspace_destroy(this);
    }

    internal void apply_pending() {
        freeze_notify();
        if (pending_id != null) {
            id = (owned)pending_id;
        }
        if (pending_name != null) {
            name = (owned)pending_name;
        }
        if (pending_coordinates != null) {
            coordinates = (owned)pending_coordinates;
        }
        if (_state != pending_state) {
            state = pending_state;
        }
        if (_capabilities != pending_capabilities) {
            capabilities = pending_capabilities;
        }
        thaw_notify();
    }
}
}
