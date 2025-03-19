namespace AstalSway {
public class Node : Object {
    public int id { get; private set; }
    public bool urgent { get; private set; }
    public string name { get; private set; }
    public string layout { get; private set; }
    public string orientation { get; private set; }
    public Rectangle rect { get; private set; }
    public Rectangle window_rect { get; private set; }
    public Rectangle deco_rect { get; private set; }
    public NodeType node_type { get; protected set;}

    public weak Node parent { get; private set; }

    public unowned List<weak Node> nodes { get; private set; }
    public List<weak Window> windows { owned get {
        var arr = new List<weak Window> ();
        foreach (var node in nodes) {
            if (node.node_type == NodeType.WINDOW) {
                arr.append(node as Window);
            } else {
                arr.concat(node.windows);
            }
        }

        return arr;
    }}
    
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
            case "con":
            case "floating_con":
                if (obj.get_member("pid") != null) {
                    var win = new Window();
                    return win as Node;
                } else {
                    var con = new Container();
                    return con as Node;
                }
            case "output":
                if (obj.get_string_member("name") == "__i3") {
                    var node = new Node();
                    node.node_type = NodeType.SCRATCHPAD;
                    return node;
                } else {
                    var output = new Output();
                    return output as Node;
                }
            default:
                var node = new Node(); 
                node.node_type = NodeType.ROOT;
                return node;
        }
    }

    internal static async void sync_tree(Json.Object obj) {
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
        urgent = obj.get_boolean_member("urgent");
        rect = Rectangle.from_json(obj.get_object_member("rect"));
        window_rect = Rectangle.from_json(obj.get_object_member("window_rect"));
        deco_rect = Rectangle.from_json(obj.get_object_member("deco_rect"));

        var arr = obj.get_array_member("nodes");
        var arr2 = obj.get_array_member("floating_nodes");
        sync_nodes(arr);
        sync_nodes(arr2);
        
        notify_property("windows");
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

         node.parent = this;
         new_nodes.append(node);
         _temp_nodes.insert(id, node);         
         node.sync(obj);
        }

        nodes = (owned) new_nodes;
    }

    public virtual void focus() {

    }

    public bool contain(Node node) {
        parent = node.parent;
        while (parent != null) {
            if (id == parent.id) {
                return true;
            }
            parent = parent.parent;
        }

        return false;
    }
}
}
