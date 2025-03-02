namespace AstalSway {
  public enum NodeType {
    ROOT,
    WORKSPACE,
    CONTAINER,
    CLIENT,
    MONITOR
  }

  public class Node : Object {
    public int id {get; private set; }
    public bool focused { get; private set; }
    public string name;
    public string layout;
    public NodeType type;
    public weak Node parent;

    private HashTable<int, Node> _nodes =
        new HashTable<int, Node>(i => i, (a,b) => a==b);
    public List<weak Node> nodes { owned get { return _nodes.get_values(); } }
    
    protected Json.Object data;

    public static Node? build(Json.Object obj) {
      if (obj == null) {
        return null;
      }
      switch (obj.get_string_member("type")) {
        case "workspace":
          var ws = new Workspace();
          ws.sync(obj);
          return ws as Node;

        default:
          var node = new Node(); 
          node.sync(obj);
          return node;
          break;

      }
      return null;
    }
    
    internal virtual void sync(Json.Object obj) {
      if (obj == null) {
        return;
      }
      data = obj;
      id = (int)obj.get_int_member("id");
      name = obj.get_string_member("name");
      focused = obj.get_boolean_member("focused");
      layout = obj.get_string_member("layout");

      var arr = obj.get_array_member("nodes");
      sync_nodes(arr);
    }

    private void sync_nodes(Json.Array arr) {
      var new_nodes = new HashTable<int, Node>(i => i, (a,b) => a==b);

      foreach (var item in arr.get_elements()) {
        var obj = item.get_object();
        int id = (int)obj.get_int_member("id");
        var node = _nodes.get(id);
        
        if (node == null) {
          node = Node.build(obj);
        }

       new_nodes.insert(id, node);     
       node.sync(obj);
      }
      
      _nodes = new_nodes;
    }
  }
}
