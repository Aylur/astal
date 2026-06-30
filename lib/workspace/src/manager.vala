namespace AstalWorkspace {
    public bool is_supported() {
        return !AstalWl.Registry.get_default().find_globals("ext_workspace_manager_v1").is_empty();
    }

    public WorkspaceManager get_default() {
        return WorkspaceManager.get_default();
    }

    public class WorkspaceManager : Object {
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

        public signal void changed();

        public WorkspaceManager() {
            var registry = AstalWl.get_default();
            var manager_global = registry.find_globals("ext_workspace_manager_v1").nth_data(0);
            if (manager_global == null) {
                critical("The compositor does not support ext_workspace_v1");
                return;
            }
            manager = registry.get_registry().bind(manager_global.name, ref ExtWorkspaceManagerV1.iface, uint.min(manager_global.version, 1));
            manager.add_listener(manager_listener, this);

            workspaces = new GenericArray<Workspace> ();
            pending_created_workspaces = new GenericArray<Workspace> ();
            pending_deleted_workspaces = new GenericArray<Workspace> ();

            groups = new GenericArray<WorkspaceGroup> ();
            pending_created_groups = new GenericArray<WorkspaceGroup> ();
            pending_deleted_groups = new GenericArray<WorkspaceGroup> ();
        }

        private void handle_done() {
            print("done\n");

            var workspaces_changed = false;
            if (pending_created_workspaces.length > 0) {
                workspaces.extend_and_steal((owned) pending_created_workspaces);
                pending_created_workspaces = new GenericArray<Workspace> ();
                workspaces_changed = true;
            }
            if (pending_deleted_workspaces.length > 0) {
                foreach (var deleted in pending_deleted_workspaces) {
                    workspaces.remove(deleted);
                }
                pending_deleted_workspaces = new GenericArray<Workspace> ();
                workspaces_changed = true;
            }
            foreach (var workspace in workspaces) {
                workspace.apply_pending();
            }

            var groups_changed = false;
            if (pending_created_groups.length > 0) {
                groups.extend_and_steal((owned) pending_created_groups);
                pending_created_groups = new GenericArray<WorkspaceGroup> ();
                groups_changed = true;
            }
            if (pending_deleted_groups.length > 0) {
                foreach (var deleted in pending_deleted_groups) {
                    groups.remove(deleted);
                }
                pending_deleted_groups = new GenericArray<WorkspaceGroup> ();
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
            changed();
        }

        private void handle_finished() {
            // TODO: should this exist?
            warning("ext-workspace finished");
        }

        private void handle_workspace_group(ExtWorkspaceManagerV1 manager, ExtWorkspaceGroupHandleV1 group) {
            print("group %p\n", group);
            pending_created_groups.add(new WorkspaceGroup(this, group));
        }

        private void handle_workspace_create(ExtWorkspaceManagerV1 manager, ExtWorkspaceHandleV1 workspace) {
            print("workspace %p\n", workspace);
            pending_created_workspaces.add(new Workspace(this, workspace));
        }

        internal void handle_group_destroy(WorkspaceGroup group) {
            if (!pending_created_groups.remove(group)) {
                pending_deleted_groups.add(group);
            }
        }

        internal void handle_workspace_destroy(Workspace workspace) {
            if (!pending_created_workspaces.remove(workspace)) {
                pending_deleted_workspaces.add(workspace);
            }
        }
    }
}
