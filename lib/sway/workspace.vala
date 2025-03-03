namespace AstalSway {
  public class Workspace : Node {
    public string representation;

    public Workspace() {
      node_type = NodeType.WORKSPACE;
    }

    internal override void sync(Json.Object obj) {
      var rep = obj.get_member("representation");
      if (rep != null) {
        representation = rep.get_string();
      } else {
        representation = "";
        print("aa");
      }
      base.sync(obj);
    }
  }
}
