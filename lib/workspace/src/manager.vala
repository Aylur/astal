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

    private int autocommit_freeze_count;
    private bool autocommit_pending;

    /**
     * Emitted when the compositor updates the workspace state.
     */
    public signal void updated();

    /**
     * Emitted when any group enters an output.
     */
    internal signal void group_enter_output(WorkspaceGroup group, AstalWl.Output output);

    /**
     * Emitted when any group leaves an output, and just before a group with outputs assigned to it gets destroyed.
     */
    internal signal void group_leave_output(WorkspaceGroup group, AstalWl.Output output);

    /**
     * Emitted when a new output appears on AstalWl with a name attached. Used by monitorviews to pair themselves with GDK monitors
     */
    internal signal void add_named_output(AstalWl.Output output);

    /**
     * Emitted when an output disappears from AstalWl. Used by monitorviews to know when to invalidate
     */
    internal signal void remove_output(AstalWl.Output output);

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
    public WorkspaceMonitorView for_output(AstalWl.Output output) {
        return new WorkspaceMonitorView(this, output);
    }

    // Having GDK linked in makes AstalWl use it (and print a critical if there is no display).
    // Pretty much everyone will have it linked in anyway, EXCEPT the astal-workspace CLI.
    // So add an escape hatch to stop depending on it.
#if !NO_GTK

    /**
     * Get a proxy object which filters workspaces to those that belong to a group on the specified GDK monitor.
     * Note that the workspaces may not appear in the proxy object immediately if the monitor is very new.
     */
    public WorkspaceMonitorView for_monitor(Gdk.Monitor monitor) {
        return new WorkspaceMonitorView.with_gdkmonitor(this, monitor);
    }

#endif

    /**
     * Commit any pending workspace method calls.
     * By default, this is done automatically; you can use freeze_autocommit()
     * to delay committing any changes until thaw_autocommit() is called.
     * You can also call this while autocommits are frozen to commit manually.
     */
    public void commit() {
        manager.commit();
    }

    /**
     * Temporarily suspend the automatic commit() calls after doing workspace actions.
     * In effect, calling methods like activate() on workspaces or create_workspace() on groups
     * while autocommitting is frozen will make them take effect all at once upon calling
     * thaw_autocommit().
     */
    public void freeze_autocommit() {
        autocommit_freeze_count++;
    }

    /**
     * Resume the automatic commit() calls after doing workspace actions.
     * If any autocommitting actions were taken during the freeze,
     * they will all be applied at once.
     */
    public void thaw_autocommit() {
        if (autocommit_freeze_count <= 0) {
            critical("unbalanced freeze/thaw_autocommit on WorkspaceManager");
        } else {
            autocommit_freeze_count--;
        }
        if (autocommit_pending) {
            autocommit_pending = false;
            commit();
        }
    }

    internal void _autocommit() {
        if (autocommit_freeze_count > 0) {
            autocommit_pending = true;
        } else {
            commit();
        }
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

        registry.output_added.connect((output) => {
                if (output.name != null) {
                    add_named_output(output);
                } else {
                    ulong id = 0;
                    id = output.notify["name"].connect(() => {
                        add_named_output(output);
                        output.disconnect(id);
                    });
                }
            });
        registry.output_removed.connect((output) => remove_output(output));

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
        warning("ext-workspace finished, but it was never requested! Further workspace updates will not occur.");
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
        // Take a reference on the workspace, so that it doesn't get destroyed after we remove it from all the containers
        // it's in (function parameters are unowned, so the workspace variable here does NOT keep it alive)
        workspace.ref();
        if (!pending_created_workspaces.remove(workspace)) {
            pending_deleted_workspaces.add(workspace);
        }
        // Compositors are supposed to remove workspaces from groups first.
        // But not all of them do. So also remove manually.
        foreach (var group in groups) {
            group.handle_workspace_leave(group._get_handle(), workspace._get_handle());
        }
        foreach (var group in pending_created_groups) {
            group.handle_workspace_leave(group._get_handle(), workspace._get_handle());
        }
        // deleted groups will get deleted anyway
        workspace.unref();
    }
}
}