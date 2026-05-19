namespace AstalWorkspace {
    [Flags]
    public enum WorkspaceState {
        ACTIVE,
        URGENT,
        HIDDEN,
    }

    [Flags]
    public enum WorkspaceCapabilities {
        ACTIVATE,
        DEACTIVATE,
        REMOVE,
        ASSIGN,
    }

    public class Workspace : Object {
        private WorkspaceManager manager;
        private ExtWorkspaceHandleV1 handle;

        private const ExtWorkspaceHandleV1Listener listener = {
            handle_id,
            handle_name,
            handle_coordinates,
            handle_state,
            handle_capabilities,
            handle_removed,
        };

        public string? id { get; private set; default = null; }
        private string? pending_id = null;
        public string name { get; private set; }
        private string? pending_name = null;
        // TODO: this probably has to be a fancier array type for bindings
        public uint32[]? coordinates { get; private set; default = null; }
        private uint32[]? pending_coordinates;
        public WorkspaceState state { get; private set; }
        private WorkspaceState pending_state;
        public WorkspaceCapabilities capabilities { get; private set; }
        private WorkspaceCapabilities pending_capabilities;

        public void activate() {
            handle.activate();
        }

        public void deactivate() {
            handle.deactivate();
        }

        public void assign(void* group) {
            // TODO
        }

        public void remove() {
            handle.remove();
        }

        internal Workspace(WorkspaceManager manager, owned ExtWorkspaceHandleV1 handle) {
            this.manager = manager;
            this.handle = (owned)handle;
            handle.add_listener(listener, this);
        }

        private void handle_id(ExtWorkspaceHandleV1 handle, string id) {
            pending_id = id;
        }
        private void handle_name(ExtWorkspaceHandleV1 handle, string name) {
            pending_name = name;
        }
        private void handle_coordinates(ExtWorkspaceHandleV1 handle, Wl.Array coordinates) {
            // TODO
        }
        private void handle_state(ExtWorkspaceHandleV1 handle, ExtWorkspaceHandleV1State state) {
            pending_state = (WorkspaceState)state;
        }
        private void handle_capabilities(ExtWorkspaceHandleV1 handle, ExtWorkspaceHandleV1WorkspaceCapabilities capabilities) {
            pending_capabilities = (WorkspaceCapabilities)capabilities;
        }
        private void handle_removed(ExtWorkspaceHandleV1 handle) {
            // TODO: tell the manager to pending-remove us
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
