namespace AstalNiri {
public class Workspace : Object {
    public uint64 id { get; private set; }
    public uint8 idx { get; private set; }
    public string? name { get; private set; }
    public string? output { get; private set; }
    public bool is_active { get; internal set; }
    public bool is_focused { get; internal set; }

    internal uint64? active_window_id;

    public signal void activated();
    public signal void active_window_changed();

    internal Workspace.from_json(Json.Object object) {
        id = object.get_int_member("id");
        idx = (uint8) object.get_int_member("idx");
        name = object.get_string_member("name");
        output = object.get_string_member("output");
        is_active = object.get_boolean_member("is_active");
        is_focused = object.get_boolean_member("is_focused");

        var w = object.get_member("active_window_id");
        if (w.is_null()) {
            active_window_id = null;
        } else {
            active_window_id = w.get_int();
        }
    }

    public unowned Window? get_active_window() {
        var i = active_window_id;
        if (i == null)
            return null;
        return Niri.get_default().get_window(i);
    }
}
}
