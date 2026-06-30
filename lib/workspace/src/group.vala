namespace AstalWorkspace {
    [Flags]
    public enum GroupCapabilities {
        CREATE_WORKSPACE,
    }

    public class WorkspaceGroup : Object {
        private WorkspaceManager manager;
        // Needs to be internal and not private for Workspace.assign_to_group
        internal unowned ExtWorkspaceGroupHandleV1 handle;

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

        public GenericArray<AstalWl.Output> outputs { get; private set; }
        private GenericArray<AstalWl.Output> pending_added_outputs;
        private GenericArray<AstalWl.Output> pending_removed_outputs;

        public GenericArray<Workspace> workspaces { get; private set; }
        private GenericArray<Workspace> pending_added_workspaces;
        private GenericArray<Workspace> pending_removed_workspaces;

        public void create_workspace(string name) {
            handle.create_workspace(name);
        }

        internal WorkspaceGroup(WorkspaceManager manager, ExtWorkspaceGroupHandleV1 handle) {
            this.manager = manager;
            this.handle = handle;
            handle.add_listener(listener, this);

            outputs = new GenericArray<AstalWl.Output> ();
            pending_added_outputs = new GenericArray<AstalWl.Output> ();
            pending_removed_outputs = new GenericArray<AstalWl.Output> ();

            workspaces = new GenericArray<Workspace> ();
            pending_added_workspaces = new GenericArray<Workspace> ();
            pending_removed_workspaces = new GenericArray<Workspace> ();
        }

        public override void dispose() {
            handle.destroy();
        }

        private void handle_capabilities(ExtWorkspaceGroupHandleV1 handle, ExtWorkspaceGroupHandleV1GroupCapabilities capabilities) {
            pending_capabilities = (GroupCapabilities) capabilities;
        }

        private void handle_output_enter(ExtWorkspaceGroupHandleV1 handle, Wl.Output wl_output) {
            var output = AstalWl.get_default().get_output_by_wl_output(wl_output);
            if (output == null) {
                warning("Couldn't find AstalWl.Output for output %p", wl_output);
                return;
            }
        }

        private void handle_output_leave(ExtWorkspaceGroupHandleV1 handle, Wl.Output wl_output) {
            var output = AstalWl.get_default().get_output_by_wl_output(wl_output);
            if (output == null) {
                warning("Couldn't find AstalWl.Output for output %p", wl_output);
                return;
            }
        }

        private void handle_workspace_enter(ExtWorkspaceGroupHandleV1 handle, ExtWorkspaceHandleV1 workspace_handle) {
            var workspace = (Workspace) workspace_handle.get_user_data();
            pending_added_workspaces.add(workspace);
        }

        private void handle_workspace_leave(ExtWorkspaceGroupHandleV1 handle, ExtWorkspaceHandleV1 workspace_handle) {
            var workspace = (Workspace) workspace_handle.get_user_data();
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
                outputs.extend_and_steal((owned) pending_added_outputs);
                pending_added_outputs = new GenericArray<AstalWl.Output> ();
                outputs_changed = true;
            }
            if (pending_removed_outputs.length > 0) {
                foreach (var deleted in pending_removed_outputs) {
                    outputs.remove(deleted);
                }
                pending_removed_outputs = new GenericArray<AstalWl.Output> ();
                outputs_changed = true;
            }

            var workspaces_changed = false;
            if (pending_added_workspaces.length > 0) {
                workspaces.extend_and_steal((owned) pending_added_workspaces);
                pending_added_workspaces = new GenericArray<Workspace> ();
                workspaces_changed = true;
            }
            if (pending_removed_workspaces.length > 0) {
                foreach (var deleted in pending_removed_workspaces) {
                    workspaces.remove(deleted);
                }
                pending_removed_workspaces = new GenericArray<Workspace> ();
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
