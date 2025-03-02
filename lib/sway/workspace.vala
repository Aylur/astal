namespace AstalSway {
  public class Workspace : Node {
    public int num {get; private set; }

    public NodeType type = NodeType.WORKSPACE;

    internal override void sync(Json.Object obj) {
      base.sync(obj);
      // num = (int)obj.get_int_member("num");
    }
  }
}
