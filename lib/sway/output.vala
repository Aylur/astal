namespace AstalSway {
public class Output : Node {
    public string make;
    public string model;
    public string serial;
    public bool active;
    public bool power;
    public bool focused;
    public float scale;
    public string subpixel_hinting;
    public string transform;
    public Workspace current_workspace;
    public List<weak Workspace> workspaces { owned get {
        var arr = new List<weak Workspace> ();
        foreach (var node in nodes) {
            arr.append(node as Workspace);
        }
        return arr;
    }}

    public Output() {
        node_type = NodeType.OUTPUT;
    }

    internal override void sync(Json.Object obj) {
        base.sync(obj);
    }

    internal void sync_output(Json.Object obj) {
        make = obj.get_string_member("make");
        model = obj.get_string_member("model");
        serial = obj.get_string_member("serial");
        transform = obj.get_string_member("transform");
        subpixel_hinting = obj.get_string_member("subpixel_hinting");
        active = obj.get_boolean_member("active");
        power = obj.get_boolean_member("power");
        focused = obj.get_boolean_member("focused");
        scale = (float)obj.get_double_member("scale");
        current_workspace = Sway.get_default().get_workspace(obj.get_string_member("current_workspace"));
    }

    public override void focus() {
        Sway.get_default().run_command(@"focus output $name");
    }
}
}
