namespace AstalSway {
  public class Container : Node {
    public bool sticky;
    public bool floating;
    public string border;
    public int border_width;
    public int fullscreen_mode;

    public Container() {
      node_type = NodeType.CONTAINER;
    }

    internal override void sync(Json.Object obj) {
      border = obj.get_string_member("border");
      border_width = (int)obj.get_int_member("current_border_width");
      sticky = obj.get_boolean_member("sticky");
      floating = obj.get_string_member("type") == "floating_con";
      fullscreen_mode = (int)obj.get_int_member("fullscreen_mode");
      base.sync(obj);
    }
    
    public override void focus() {
      Sway.get_default().run_command(@"[con_id=$id] focus");
    }
  }
}
