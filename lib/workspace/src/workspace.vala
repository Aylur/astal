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
        private unowned ExtWorkspaceHandleV1 handle;

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
        public GenericArray<uint32>? coordinates { get; private set; default = null; }
        private GenericArray<uint32>? pending_coordinates;
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

        internal Workspace(WorkspaceManager manager, ExtWorkspaceHandleV1 handle) {
            this.manager = manager;
            this.handle = handle;
            handle.add_listener(listener, this);
        }

        public override void dispose() {
            this.handle.destroy();
        }

        private void handle_id(ExtWorkspaceHandleV1 handle, string id) {
            pending_id = id;
        }
        private void handle_name(ExtWorkspaceHandleV1 handle, string name) {
            pending_name = name;
        }
        private void handle_coordinates(ExtWorkspaceHandleV1 handle, Wl.Array coordinates) {
            uint32 *coords_data = coordinates.data;
            var count = coordinates.size / sizeof(uint32);
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
