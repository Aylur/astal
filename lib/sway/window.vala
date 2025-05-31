namespace AstalSway {
public class Window : Container {
    public string app_id { get; private set; } 
    public string shell { get; private set; } 
    public int pid { get; private set; }
    public bool visible { get; private set; }
    public bool inhibit_idle { get; private set; }
        
    public Window() {
        node_type = NodeType.WINDOW;
    }

    internal override void sync(Json.Object obj) {
        app_id = obj.get_string_member("app_id");
        shell = obj.get_string_member("shell");
        pid = (int)obj.get_int_member("pid");
        visible = obj.get_boolean_member("visible");
        inhibit_idle = obj.get_boolean_member("inhibit_idle");
        base.sync(obj);
    }
}
}

