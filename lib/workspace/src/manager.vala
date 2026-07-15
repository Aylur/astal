namespace AstalWorkspace {
/** Check whether the ext-workspace-v1 protocol is supported. */
public bool is_supported() {
    return !AstalWl.Registry.get_default().find_globals("ext_workspace_manager_v1").is_empty();
}

public WorkspaceManager get_default() {
    return WorkspaceManager.get_default();
}

/**
 * The global workspace state. As a ListModel, it exposes every workspace available;
 * lists of workspaces and groups are also available through properties,
 * and helper objects can be created to access workspaces on a specific monitor
 * through the for_monitor and for_output factory methods.
 */
public class WorkspaceManager : Object, ListModel {
    private static WorkspaceManager? instance;

    public static WorkspaceManager get_default() {
        if (instance == null) {
            instance = new WorkspaceManager();
        }
        return instance;
    }

    private const ExtWorkspaceManagerV1Listener manager_listener = {
        handle_workspace_group,
        handle_workspace_create,
        handle_done,
        handle_finished,
    };

    private ExtWorkspaceManagerV1? manager;

    public GenericArray<Workspace> workspaces { get; private set; }
    private GenericArray<Workspace> pending_created_workspaces;
    private GenericArray<Workspace> pending_deleted_workspaces;

    public GenericArray<WorkspaceGroup> groups { get; private set; }
    private GenericArray<WorkspaceGroup> pending_created_groups;
    private GenericArray<WorkspaceGroup> pending_deleted_groups;

    /**
     * Emitted when the compositor updates the workspace state.
     */
    public signal void updated();

    /**
     * Emitted when any group enters an output.
     */
    public signal void group_enter_output(WorkspaceGroup group, AstalWl.Output output);

    /**
     * Emitted when any group leaves an output, and just before a group with outputs assigned to it gets destroyed.
     */
    public signal void group_leave_output(WorkspaceGroup group, AstalWl.Output output);

    /**
     * Get the workspace at a specific position in the list, or null if out-of-bounds.
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
     * Get a proxy object which filters workspaces to those that belong to a group on the specified Wayland output.
     */
    public MonitorView for_output(AstalWl.Output output) {
        return new MonitorView(this, output);
    }

    // Having GDK linked in makes AstalWl use it (and print a critical if there is no display).
    // Pretty much everyone will have it linked in anyway, EXCEPT the astal-workspace CLI.
    // So add an escape hatch to stop depending on it.
#if !NO_GTK

    /**
     * Get a proxy object which filters workspaces to those that belong to a group on the specified GDK monitor.
     */
    public MonitorView for_monitor(Gdk.Monitor monitor) {
        var wl_monitor = monitor as Gdk.Wayland.Monitor;
        return_val_if_fail(wl_monitor != null, null);

        var output = AstalWl.get_default().get_output_by_wl_output(wl_monitor.get_wl_output());
        return new MonitorView(this, output);
    }

#endif

    /**
     * Commit any pending workspace method calls.
     * After calling any of the workspace methods (activate, deactivate, assign, and remove)
     * this method needs to be called for them to apply.
     */
    public void commit() {
        manager.commit();
    }

    public WorkspaceManager() {
        var registry = AstalWl.get_default();
        var manager_global = registry.find_globals("ext_workspace_manager_v1").nth_data(0);
        if (manager_global == null) {
            critical("The compositor does not support ext_workspace_v1");
            return;
        }
        manager = registry.get_registry().bind(manager_global.name, ref ExtWorkspaceManagerV1.iface, uint.min(manager_global.version, 1));
        manager.add_listener(manager_listener, this);

        workspaces = new GenericArray<Workspace>();
        pending_created_workspaces = new GenericArray<Workspace>();
        pending_deleted_workspaces = new GenericArray<Workspace>();

        groups = new GenericArray<WorkspaceGroup>();
        pending_created_groups = new GenericArray<WorkspaceGroup>();
        pending_deleted_groups = new GenericArray<WorkspaceGroup>();
    }

    private void handle_done() {
        debug("manager done");

        // This is done in a very particular order, to make emitting items-changed as easy as possible,
        // and to ensure every remaining workspace has apply_pending called on it exactly once before it's exposed.
        // First, items are deleted, from highest index to lowest, then apply_pending is called on the remaining items,
        // and then the pending created items have their pending state applied and are added all at once.
        var workspaces_changed = false;
        var deleted_count = pending_deleted_workspaces.length;
        if (deleted_count > 0) {
            var deleted_indices = new GenericArray<uint>(deleted_count);
            for (int i = 0; i < deleted_count; i++) {
                uint index;
                if (workspaces.find(pending_deleted_workspaces[i], out index)) {
                    deleted_indices.add(index);
                } else {
                    critical("Couldn't find workspace to delete");
                }
            }
            deleted_indices.sort((a, b) => (int)a - (int)b);

            for (int i = deleted_indices.length - 1; i >= 0; i--) {
                uint run_end = deleted_indices[i];
                while (i > 0 && deleted_indices[i - 1] == deleted_indices[i] - 1) {
                    i--;
                }
                uint run_start = deleted_indices[i];
                // Items in the range [run_start, run_end] can be deleted all at once.
                uint run_length = run_end + 1 - run_start;
                workspaces.remove_range(run_start, run_length);
                items_changed(run_start, run_length, 0);
            }

            pending_deleted_workspaces = new GenericArray<Workspace>();
            workspaces_changed = true;
        }
        foreach (var workspace in workspaces) {
            workspace.apply_pending();
        }
        if (pending_created_workspaces.length > 0) {
            foreach (var created in pending_created_workspaces) {
                created.apply_pending();
            }

            uint before_length = workspaces.length;
            uint count = pending_created_workspaces.length;
            workspaces.extend_and_steal((owned)pending_created_workspaces);
            pending_created_workspaces = new GenericArray<Workspace>();

            items_changed(before_length, 0, count);
            workspaces_changed = true;
        }

        // Groups are done after workspaces so that monitorviews' workspaces have correct contents.
        var groups_changed = false;
        if (pending_created_groups.length > 0) {
            groups.extend_and_steal((owned)pending_created_groups);
            pending_created_groups = new GenericArray<WorkspaceGroup>();
            groups_changed = true;
        }
        if (pending_deleted_groups.length > 0) {
            foreach (var deleted in pending_deleted_groups) {
                foreach (var output in deleted.outputs) {
                    group_leave_output(deleted, output);
                }
                groups.remove(deleted);
            }
            pending_deleted_groups = new GenericArray<WorkspaceGroup>();
            groups_changed = true;
        }
        foreach (var group in groups) {
            group.apply_pending();
        }

        if (workspaces_changed) {
            notify_property("workspaces");
        }
        if (groups_changed) {
            notify_property("groups");
        }
        updated();
    }

    private void handle_finished() {
        // TODO: should this exist?
        warning("ext-workspace finished");
    }

    private void handle_workspace_group(ExtWorkspaceManagerV1 manager, ExtWorkspaceGroupHandleV1 group) {
        debug("create group %p", group);
        pending_created_groups.add(new WorkspaceGroup(this, group));
    }

    private void handle_workspace_create(ExtWorkspaceManagerV1 manager, ExtWorkspaceHandleV1 workspace) {
        debug("create workspace %p", workspace);
        pending_created_workspaces.add(new Workspace(this, workspace));
    }

    internal void handle_group_destroy(WorkspaceGroup group) {
        debug("destroy group %p", group);
        if (!pending_created_groups.remove(group)) {
            pending_deleted_groups.add(group);
        }
    }

    internal void handle_workspace_destroy(Workspace workspace) {
        debug("destroy workspace %p", workspace);
        if (!pending_created_workspaces.remove(workspace)) {
            pending_deleted_workspaces.add(workspace);
        }
    }
}
}