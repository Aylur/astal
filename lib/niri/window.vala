namespace AstalNiri {
public class Window : Object {
    public uint64 id { get; private set; }
    public string? title { get; private set; }
    public string? app_id { get; private set; }
    public bool is_focused { get; internal set; }

    internal uint64? workspace_id;

    public signal void changed();
    public signal void closed();

    internal Window.from_json(Json.Object object) {
        sync(object);
    }

    public unowned Workspace? get_workspace() {
        var i = workspace_id;
        if (i == null)
            return null;
        return Niri.get_default().get_workspace(i);
    }

    internal void sync(Json.Object object) {
        id = object.get_int_member("id");
        title = object.get_string_member("title");
        app_id = object.get_string_member("app_id");
        is_focused = object.get_boolean_member("is_focused");

        var w = object.get_member("workspace_id");
        if (w.is_null()) {
            workspace_id = null;
        } else {
            workspace_id = w.get_int();
        }
    }
}
}
