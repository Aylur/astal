namespace AstalNiri {
public class Workspace : Object {
    /** unique id of workspace */
    public uint64 id { get; private set; }
    /** index of the workspace on its monitor */
    public uint8 idx { get; private set; }
    /** optional name of the workspace */
    public string? name { get; private set; }
    /** name of the output the workspace is on */
    public string? output { get; internal set; }
    /** if a window on the workspace is urgent */
    public bool is_urgent { get; internal set; }
    /** if the workspace is active on its outpput */
    public bool is_active { get; internal set; }
    /** if this is the current Focused Workspace */
    public bool is_focused { get; internal set; }
    /** id of the active window on the workspace */
    public uint64 active_window_id {get; internal set;}

    public unowned Window? active_window { get {
        return Niri.get_default().get_window(active_window_id);
    } }

    public List<weak Window> windows { owned get {
        var list = new List<weak Window>();
        Niri.get_default().windows.foreach((val) => {
            if(val.workspace_id == id) {
                list.append(val);
            }
        });
        list.sort(sort_windows);
        return list.copy();
    } }

    private static int sort_windows(Window a, Window  b) {
        // order all tiled windows first
        // then sort floating by ??? yoloing with opened order
        if (a.is_floating == false && b.is_floating == false) {
            var x = (int)a.layout.pos_in_scrolling_layout[0] - (int)b.layout.pos_in_scrolling_layout[0];
            if (x != 0) return x;
            var y = (int)a.layout.pos_in_scrolling_layout[1] - (int)b.layout.pos_in_scrolling_layout[1];
            if (y != 0) return y;
            return 0;
        } else if(a.is_floating != b.is_floating) {
            if (b.is_floating) return -1;
            // a must be floating
            return 1;
        } else {
            // both windows are floating
            return (a.id > b.id) ? 1 : -1;
        }
    }

    /** Emitted when a workspace was activated on an output. */
    public signal void activated();
    /** Emitted when the window changes on a workspace. */
    public signal void active_window_changed(uint64? id);

    internal Workspace.from_json(Json.Object object) {
        sync(object);
    }

    internal void sync(Json.Object object) {
        id = object.get_int_member("id");
        idx = (uint8) object.get_int_member("idx");
        var _name = object.get_member("name");
        var _output = object.get_member("output");
        is_urgent = object.get_boolean_member("is_urgent");
        is_active = object.get_boolean_member("is_active");
        is_focused = object.get_boolean_member("is_focused");
        var _active_window_id = object.get_member("active_window_id");

        if (_active_window_id.is_null()) { active_window_id = 0;}
        else { active_window_id = _active_window_id.get_int(); }

        if (_name.is_null()) { name = null;}
        else { name = _name.get_string(); }

        if (_output.is_null()) { output = null;}
        else { output = _output.get_string(); }
    }

    public bool focus() {
       return msg.focus_workspace_by_id((int) id);
    }
    public bool rename(string name) {
        return msg.set_workspace_name_by_id((int) id, name);
    }
    public bool move_to_monitor(string output) {
        return msg.move_workspace_to_monitor_by_id(output, (int) id);
    }
}
}
