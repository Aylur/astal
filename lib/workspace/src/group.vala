namespace AstalWorkspace {
    [Flags]
    public enum GroupCapabilities {
        CREATE_WORKSPACE,
    }

    public class WorkspaceGroup : Object {
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

        public GroupCapabilities capabilities { get; private set; }
        private GroupCapabilities pending_capabilities;

        public void create_workspace(string name) {
            handle.create_workspace(name);
        }

        internal WorkspaceGroup(WorkspaceManager manager, ExtWorkspaceGroupHandleV1 handle) {
            this.manager = manager;
            this.handle = handle;
            handle.add_listener(listener, this);
        }

        public override void dispose() {
            handle.destroy();
        }

        private void handle_capabilities(ExtWorkspaceGroupHandleV1 handle, ExtWorkspaceGroupHandleV1GroupCapabilities capabilities) {
            pending_capabilities = (GroupCapabilities)capabilities;
        }
        private void handle_output_enter(ExtWorkspaceGroupHandleV1 handle, Wl.Output output) {}
        private void handle_output_leave(ExtWorkspaceGroupHandleV1 handle, Wl.Output output) {}
        private void handle_workspace_enter(ExtWorkspaceGroupHandleV1 handle, ExtWorkspaceHandleV1 workspace) {}
        private void handle_workspace_leave(ExtWorkspaceGroupHandleV1 handle, ExtWorkspaceHandleV1 workspace) {}
        private void handle_removed(ExtWorkspaceGroupHandleV1 handle) {
            manager.handle_group_destroy(this);
        }

        internal void apply_pending() {
            freeze_notify();
            if (_capabilities != pending_capabilities) {
                capabilities = pending_capabilities;
            }
            thaw_notify();
        }
    }
}
