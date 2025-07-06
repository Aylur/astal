namespace AstalNiri {
public class Window : Object {
    /** unique id of window*/
    public uint64 id { get; private set; }
    /** name of the window, if available */
    public string? title { get; private set; }
    /** app_id of the window, if available  */
    public string? app_id { get; private set; }
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

    public unowned Workspace? workspace { get {
        return Niri.get_default().get_workspace(workspace_id);
    } }

    internal void sync(Json.Object object) {
        id = object.get_int_member("id");
        var _title = object.get_member("title");
        var _app_id = object.get_member("app_id");
        var _workspace_id = object.get_member("workspace_id");
        is_urgent = object.get_boolean_member("is_urgent");
        is_focused = object.get_boolean_member("is_focused");

        if (_title.is_null()) { title = null;}
        else if(title != _title.get_string()) { title = _title.get_string(); }

        if (_app_id.is_null()) { app_id = null;}
        else if(app_id != _app_id.get_string()) { app_id = _app_id.get_string(); }

        if (_workspace_id.is_null()) { workspace_id = 0; }
        else {
            var new_workspace_id = _workspace_id.get_int();
            if(workspace_id != new_workspace_id) {
                var prev_workspace = workspace;
                workspace_id = new_workspace_id;
                prev_workspace?.notify_property("windows");
                workspace?.notify_property("windows");
            }
        }
    }

    // public bool focus(string app_id) {
    // no focus_window_by_id in ipc. needs wlr-toplevel protocol
    // }

    public bool set_urgency(bool new_urgency) {
        if (is_urgent == new_urgency) return true;
        return msg.toggle_window_urgent((int) id);
    }

    public bool move_to_workspace(int workspace_id, bool? focus = false) {
        return msg.move_window_to_workspace_by_id((int) id, workspace_id, focus);
    }

    public bool move_to_monitor(string output) {
        return msg.move_window_to_monitor((int) id, output);
    }
}
}
