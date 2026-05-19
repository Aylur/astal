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

        private List<Workspace> _workspaces;
        private List<Workspace> pending_created_workspaces;
        public List<weak Workspace> workspaces { owned get { return _workspaces.copy(); } }

        public WorkspaceManager() {
            var registry = AstalWl.get_default();
            var manager_global = registry.find_globals("ext_workspace_manager_v1").nth_data(0);
            if (manager_global == null) {
                critical("The compositor does not support ext_workspace_v1");
                return;
            }
            manager = registry.get_registry().bind(manager_global.name, ref ExtWorkspaceManagerV1.iface, uint.min(manager_global.version, 1));
            manager.add_listener(manager_listener, this);

            _workspaces = new List<Workspace>();
            pending_created_workspaces = new List<Workspace>();
        }

        private void handle_done() {
            print("done\n");
            if (!pending_created_workspaces.is_empty()) {
                _workspaces.concat((owned) pending_created_workspaces);
                pending_created_workspaces = new List<Workspace>();
                notify_property("workspaces");
            }

            foreach (var workspace in _workspaces) {
                workspace.apply_pending();
            }
        }

        private void handle_finished() {
            // TODO: should this exist?
            warning("ext-workspace finished");
        }

        private void handle_workspace_group(ExtWorkspaceManagerV1 manager, ExtWorkspaceGroupHandleV1 group) {
            print("group %p\n", group);
        }

        private void handle_workspace_create(ExtWorkspaceManagerV1 manager, ExtWorkspaceHandleV1 workspace) {
            print("workspace %p\n", workspace);
            pending_created_workspaces.append(new Workspace(this, workspace));
        }
    }
}
