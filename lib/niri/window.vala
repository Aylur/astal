namespace AstalNiri {
public class Window : Object {
    /** unique id of window*/
    public uint64 id { get; private set; }
    /** name of the window, if available */
    public string? title { get; private set; }
    /** app_id of the window, if available  */
    public string? app_id { get; private set; }
    // TODO: move window to new workspace on set */
    /** workspace_id of the window, if available  */
    public uint64 workspace_id {get; private set; }
    /** if the window is requesting attention */
    public bool is_urgent { get; internal set; }
    /** if this is the current Focused Window */
    public bool is_focused { get; internal set; }

    public signal void changed();
    public signal void closed();

    internal Window.from_json(Json.Object object) {
        sync(object);
    }

    public unowned Workspace? get_workspace() {
        return Niri.get_default()._workspaces.get(workspace_id);
    }

    internal void sync(Json.Object object) {
        id = object.get_int_member("id");
        var _title = object.get_member("title");
        var _app_id = object.get_member("app_id");
        var _workspace_id = object.get_member("workspace_id");
        is_urgent = object.get_boolean_member("is_urgent");
        is_focused = object.get_boolean_member("is_focused");

        if (_title.is_null()) { title = null;}
        else { title = _title.get_string(); }

        if (_app_id.is_null()) { app_id = null;}
        else { app_id = _app_id.get_string(); }

        if (_workspace_id.is_null()) { workspace_id = -1; }
        else { workspace_id = _workspace_id.get_int(); }
    }
}
}
