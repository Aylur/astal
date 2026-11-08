void main() {
    int next = int.MIN;
    assert(AstalHyprland.WorkspaceIdentity.take_handle(ref next) == int.MIN);
    assert(next == int.MIN + 1);
    next = -1;
    assert(AstalHyprland.WorkspaceIdentity.take_handle(ref next) == -1);
    assert(next == 0);
    assert(AstalHyprland.WorkspaceIdentity.take_handle(ref next) == null);
    assert(next == 0);
}
