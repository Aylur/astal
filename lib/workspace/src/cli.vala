void print_state(AstalWorkspace.WorkspaceManager manager) {
    print("Workspaces:\n");
    foreach (var workspace in manager.workspaces) {
        print("{\n");
        print("  id: %s\n", workspace.id);
        print("  name: %s\n", workspace.name);
        print("  state: %d\n", workspace.state);
        print("  capabilities: %d\n", workspace.capabilities);
        if (workspace.coordinates != null) {
            print("  coordinates: [");
            for (var i = 0; i < workspace.coordinates.length; i++) {
                if (i > 0) {
                    print(", ");
                }
                print("%u", workspace.coordinates[i]);
            }
            print("]\n");
        }
        print("}\n");
    }
}

int main(string[] args) {
    if (!AstalWorkspace.is_supported()) {
        error("Workspace protocol not supported");
    }

    var manager = new AstalWorkspace.WorkspaceManager();
    print("manager: %p\n", manager);
    AstalWl.get_default().get_display().roundtrip();
    print_state(manager);
    manager.notify["workspaces"].connect(() => print_state(manager));

    var loop = new MainLoop();
    loop.run();

    return 0;
}
