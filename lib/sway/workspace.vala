namespace AstalSway {
public class Workspace : Node {
    public string representation { get; private set; }
    public bool focused { get; private set; }
    public bool visible { get; private set; }
    public int num { get; private set; }

    public Workspace() {
        node_type = NodeType.WORKSPACE;
    }

    internal override void sync(Json.Object obj) {
        var rep = obj.get_member("representation");
        if (rep != null) {
            representation = rep.get_string();
        } else {
            representation = "";
        }
        base.sync(obj);

    }

    internal void sync_workspace(Json.Object obj) {
        focused = obj.get_boolean_member("focused");
        visible = obj.get_boolean_member("visible");
        num = (int)obj.get_int_member("num");
    }

    public override void focus() {
        Sway.get_default().run_command(@"focus workspace $name");
    }
}
}
