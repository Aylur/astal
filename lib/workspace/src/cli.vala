int main(string[] args) {
    if (!AstalWorkspace.is_supported()) {
        error("Workspace protocol not supported");
    }

    var manager = new AstalWorkspace.WorkspaceManager();
    print("manager: %p\n", manager);
    AstalWl.get_default().get_display().roundtrip();
    print("Workspaces:\n");
    foreach (var workspace in manager.workspaces) {
        print("{\n");
        print("  id: %s\n", workspace.id);
        print("  name: %s\n", workspace.name);
        print("  state: %d\n", workspace.state);
        print("  capabilities: %d\n", workspace.capabilities);
        print("}\n");
    }

    return 0;
}
