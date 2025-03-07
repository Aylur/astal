namespace AstalSway {
    public Sway get_default() {
        return Sway.get_default();
    }


  public class Sway : Object {
    private static Sway _instance;
    private Ipc ipc;
    private SocketConnection subscribe_socket;

    public static Sway? get_default() {
      if (_instance != null) {
        return _instance;
      }
     
      
      var s = new Sway();
      try {
        s.init();
        _instance = s;
        return s;
      } catch (Error err) {
        critical(err.message);
        return null;
      }

    }

    private void init() throws Error {
      ipc = new Ipc();
      ipc.init();
      subscribe.begin(() => sync_tree.begin());
    }

    public Workspace focused_workspace;
    public Output focused_output;
    public Container? focused_container;
  
    private static HashTable<int, Node> _nodes =
        new HashTable<int, Node>(i => i, (a,b) => a==b);
    
    public List<weak Node> nodes { owned get { return _nodes.get_values(); } }
    
    private HashTable<string, Workspace> _workspaces =
        new HashTable<string, Workspace>(str_hash, str_equal);
    private HashTable<string, Output> _outputs =
        new HashTable<string, Output>(str_hash, str_equal);

    public List<weak Workspace> workspaces { owned get { return _workspaces.get_values(); } }
    public List<weak Output> outputs { owned get { return _outputs.get_values(); } }
    public List<weak Window> windows { owned get { 
      var arr = new List<weak Window>();
      foreach (var node in nodes) {
        if (node.node_type == NodeType.WINDOW) {
          arr.append(node as Window);
        }
      } 
      return arr;
    } }

    public Workspace get_workspace(string name) { return _workspaces.get(name); }
    public Output get_output(string name) { return _outputs.get(name); }
    public Node get_node(int id) { return _nodes.get(id); }
    
    ~Sway() {
        if (subscribe_socket != null) {
          try {
              subscribe_socket.close(null);
          } catch (Error err) {
              critical(err.message);
          }
        }
    }

    public string message(PayloadType type, string payload) {
      return ipc.message(type, payload);
    }
    
    public async string message_async(PayloadType type, string payload) {
      return yield ipc.message_async(type, payload);
    }

    public void run_command(string command) {
      message_async.begin(PayloadType.MESSAGE_RUN_COMMAND, command, 
        (_, res) => {
          var arr = Json.from_string(message_async.end(res)).get_array();
          foreach (var item in arr.get_elements()) {
            var obj = item.get_object();
            if (!obj.get_boolean_member("success")) {
              critical("Command error: %s", obj.get_string_member("error"));
            }
          }
        }
      );
    }

    public async void sync_tree() {
      yield Node.sync_tree();
      _nodes = Node._all_nodes;
      
      var new_workspaces = new HashTable<string, Workspace>(str_hash, str_equal);
      var new_outputs = new HashTable<string, Output>(str_hash, str_equal);

 
      // It's possible that there's no focused container
      Container? new_focused_container = null;

      foreach (var node in nodes) {
        switch (node.node_type) {
          case NodeType.WORKSPACE:
            var ws = node as Workspace;
            if (ws.name != "__i3_scratch") {
              new_workspaces.insert(ws.name, ws);
            }
            break;
          case NodeType.CONTAINER:
          case NodeType.WINDOW:
            var con =  node as Container;
            if (con.focused) {
              new_focused_container = con;
            }
            break;
          case NodeType.OUTPUT:
            var output = node as Output;
            new_outputs.insert(output.name, output);
            break;
        }
      }
      _outputs = new_outputs;
      _workspaces= new_workspaces;

      focused_container = new_focused_container;
      yield sync_workspaces();
      yield sync_outputs();
    }

    private async void sync_workspaces() {
      var res = yield message_async(PayloadType.MESSAGE_GET_WORKSPACES, "");
      var arr = Json.from_string(res).get_array();
      foreach (var item in arr.get_elements()) {
        var obj = item.get_object();
        var ws = get_workspace(obj.get_string_member("name"));
        if (ws != null) {
          ws.sync_workspace(obj);        
          if (ws.focused) {
            focused_workspace = ws;
          }
        }
      }
    }

    private async void sync_outputs() {
      var res = yield message_async(PayloadType.MESSAGE_GET_OUTPUTS, "");
      var arr = Json.from_string(res).get_array();
      foreach (var item in arr.get_elements()) {
        var obj = item.get_object();
        var output = get_output(obj.get_string_member("name"));
        if (output != null) {
          output.sync_output(obj);        
          if (output.focused) {
            focused_output = output;
          }
        }
      }
    } 

    private async void subscribe() {
      if (subscribe_socket != null) {
        return;
      }

      subscribe_socket = ipc.connection();
      ipc.send(subscribe_socket.output_stream, PayloadType.MESSAGE_SUBSCRIBE, "[ \"workspace\", \"window\" ]");
      while (true) {
        var result = yield ipc.receive_async(subscribe_socket.input_stream);
        handle_event(result);
      }
    }

    public signal void event(PayloadType type, string args);
    
    public signal void workspace_added(Workspace ws);
    public signal void workspace_removed(string name);
    public signal void workspace_focus(Workspace new_ws, Workspace old_ws);

    public signal void window_added(Window win);
    public signal void window_close(string title);
    public signal void window_move(Window win);
    public signal void window_focus(Window win);
    
    public signal void container_float(Container con);

    private async void handle_event(IpcReponse reply) {
      yield sync_tree();
      switch (reply.type) {
        case PayloadType.MESSAGE_SUBSCRIBE:
          return;
        
        case PayloadType.EVENT_WORKSPACE:
          handle_workspace_event(reply.payload);
          break;
        
        case PayloadType.EVENT_WINDOW:
          handle_window_event(reply.payload);
          break;

        case PayloadType.EVENT_OUTPUT:
          yield sync_outputs();
          break;

        default:
          break;
      }
      
      event(reply.type, reply.payload);
    }

    private void handle_workspace_event(string args) {
      var obj = Json.from_string(args).get_object();

      switch (obj.get_string_member("change")) {
        case "init": 
          var name = obj.get_object_member("current").get_string_member("name");
          workspace_added(get_workspace(name));
          notify_property("workspaces");
          break;

        case "empty":
          var name = obj.get_object_member("current").get_string_member("name");
          workspace_removed(name);
          notify_property("workspaces");
          break;

        case "focus":
          var new_name = obj.get_object_member("current").get_string_member("name");
          var old_name = obj.get_object_member("old").get_string_member("name");
          workspace_focus(get_workspace(new_name), get_workspace(old_name));
          break;
        
        default:
          break;
      }
    }

    private void handle_window_event(string args) {
      var obj = Json.from_string(args).get_object();
      var change = obj.get_string_member("change");
      var container = obj.get_object_member("container");
      int id = (int)container.get_int_member("id");

      switch (change) {
        case "new":
          window_added(get_node(id) as Window);
          notify_property("windows"); 
          break;
        case "close":
          window_close(container.get_string_member("name"));
          notify_property("windows"); 
          break;
        case "focus":
          window_focus(get_node(id) as Window);
          break;
        case "move":
          window_move(get_node(id) as Window);
          break;
        case "floating":
          container_float(get_node(id) as Container);
          break;
      }
    }
  }
}
