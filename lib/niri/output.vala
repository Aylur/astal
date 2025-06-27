namespace AstalNiri {
public class Output : Object {
    public string name { get; private set; }
    public string make { get; private set; }
    public string model { get; private set; }
    // Option<String>
    public string? serial { get; private set; }
    // Option<(u32, u32)>
    public PhysicalSize? physical_size { get; private set; }

    public Array<Mode> modes { get; private set; }
    // Option<usize> in rust. not sure what type would be best to use instead
    public int64 current_mode { get; private set; }
    public bool vrr_supported { get; private set; }
    public bool vrr_enabled { get; private set; }
    public LogicalOutput? logical { get; private set; }

    public int64 active_workspace_id { get; private set; }
    // private List<weak Workspace> _workspaces = new List<weak Workspace>();
    // public List<weak Workspace> workspaces { owned get { return _workspaces.copy(); } }

    /** Emitted when an output is activated. */
    public signal void focused(int id);
    /** Emitted when the workspace changes on an output. */
    public signal void active_workspace_changed(int64? id);

    internal Output.from_json(Json.Object object) {
        modes = new Array<Mode>();
        sync(object);
    }

    internal void sync(Json.Object object) {
        name = object.get_string_member("name");
        make = object.get_string_member("make");
        model = object.get_string_member("model");

        var _serial = object.get_member("serial");
        if (_serial.is_null())  serial = null;
        else serial = _serial.get_string();

        var _physical_size = object.get_member("physical_size");
        if (_physical_size.is_null()) physical_size = null;
        else {
            physical_size = PhysicalSize.from_json(_physical_size.get_array());
        }

        foreach (var mode in object.get_array_member("modes").get_elements()) 
            modes.append_val(Mode.from_json(mode.get_object()));
        

        var _current_mode = object.get_member("current_mode");
        if (_current_mode.is_null())  current_mode = -1;
        else current_mode = _current_mode.get_int();

        vrr_supported = object.get_boolean_member("vrr_supported");
        vrr_enabled = object.get_boolean_member("vrr_enabled");

        var _logical = object.get_member("logical");
        if (_logical.is_null())  logical = null;
        else logical = LogicalOutput.from_json(_logical.get_object());
    }

    public unowned Workspace? get_active_workspace() {
        if (active_workspace_id == -1) return null;
        return Niri.get_default().get_workspace(active_workspace_id);
    }
}

}
