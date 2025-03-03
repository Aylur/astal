namespace AstalSway {
  public enum NodeType {
    ROOT,
    WORKSPACE,
    CONTAINER,
    WINDOW,
    OUTPUT
  }

  public struct Rectangle {
    public static Rectangle from_json(Json.Object obj) {
      return Rectangle() {
        x = (int)obj.get_int_member("x"),
        y = (int)obj.get_int_member("y"),
        width = (int)obj.get_int_member("width"),
        height = (int)obj.get_int_member("height"),
      };

    }

    public int x;
    public int y;
    public int width;
    public int height;
  }

  public class Node : Object {
    public int id {get; private set; }
    public bool focused { get; private set; }
    public bool urgent { get; private set; }
    public string name { get; private set; }
    public string layout { get; private set; }
    public string orientation { get; private set; }
    public float? percent { get; private set; }
    public Rectangle rect { get; private set; }
    public Rectangle window_rect { get; private set; }
    public Rectangle deco_rect { get; private set; }
    public NodeType node_type {get; protected set;}

    public weak Node parent;

    public List<weak Node> nodes;
    
    protected Json.Object data;
    
    private static HashTable<int, Node> _temp_nodes =
        new HashTable<int, Node>(i => i, (a,b) => a==b);
    internal static HashTable<int, Node> _all_nodes =
        new HashTable<int, Node>(i => i, (a,b) => a==b);

    public static Node? build(Json.Object obj) {
      if (obj == null) {
        return null;
      }
      switch (obj.get_string_member("type")) {
        case "workspace":
          var ws = new Workspace();
          return ws as Node;

        default:
          var node = new Node(); 
          return node;
          break;

      }
      return null;
    }

    internal static async void sync_tree() {
      var str = yield Sway.get_default().message_async(PayloadType.MESSAGE_GET_TREE, "");
      var obj = Json.from_string(str).get_object();
      if (obj == null) {
        return;
      }
      
      Node root = build(obj);
      _temp_nodes = new HashTable<int, Node>(i => i, (a,b) => a==b);
      root.sync(obj);
      _temp_nodes.insert(root.id, root);
      _all_nodes = _temp_nodes;

    }
    
    internal virtual void sync(Json.Object obj) {
      if (obj == null) {
        return;
      }
      data = obj;
      id = (int)obj.get_int_member("id");
      name = obj.get_string_member("name");
      orientation = obj.get_string_member("orientation");
      layout = obj.get_string_member("layout");
      focused = obj.get_boolean_member("focused");
      urgent = obj.get_boolean_member("urgent");
      rect = Rectangle.from_json(obj.get_object_member("rect"));
      window_rect = Rectangle.from_json(obj.get_object_member("window_rect"));
      deco_rect = Rectangle.from_json(obj.get_object_member("deco_rect"));

      var arr = obj.get_array_member("nodes");
      var arr2 = obj.get_array_member("floating_nodes");
      sync_nodes(arr);
      sync_nodes(arr2);

    }

    private void sync_nodes(Json.Array arr) {
      var new_nodes = new List<weak Node> ();

      foreach (var item in arr.get_elements()) {
        var obj = item.get_object();
        int id = (int)obj.get_int_member("id");
        var node = _all_nodes.get(id);
        
        if (node == null) {
          node = Node.build(obj);
        }

       new_nodes.append(node);
       _temp_nodes.insert(id, node);     
       node.sync(obj);
      }

      nodes = (owned) new_nodes;
    }
  }
}
