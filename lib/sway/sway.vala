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
      var ipc = new Ipc();
      
s.notify.connect((_, p) => {
    stdout.printf("Property '%s' has changed!\n", p.name);
});
      try {
        ipc.init();
        s.ipc = ipc;
        s.subscribe.begin();
        _instance = s;
        return s;
      } catch (Error err) {
        critical(err.message);
        return null;
      }

    }

    public Workspace focused_workspace;
  
    private static HashTable<int, Node> _nodes =
        new HashTable<int, Node>(i => i, (a,b) => a==b);
    
    public List<weak Node> nodes { owned get { return _nodes.get_values(); } }
    
    private HashTable<string, Workspace> _workspaces =
        new HashTable<string, Workspace>(str_hash, str_equal);

    public List<weak Workspace> workspaces { owned get { return _workspaces.get_values(); } }

    public Workspace get_workspace(string name) { return _workspaces.get(name); }
    
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
          var obj = Json.from_string(message_async.end(res)).get_object();
          if (!obj.get_boolean_member("success")) {
            critical("Command error: %s", obj.get_string_member("error"));
          }
        }
      );
    }

    public async void sync() {
      yield Node.sync_tree();
      _nodes = Node._all_nodes;
      
      var new_workspaces = new HashTable<string, Workspace>(str_hash, str_equal);
      foreach (var node in nodes) {
        switch (node.node_type) {
          case NodeType.WORKSPACE:
            var ws = node as Workspace;
            new_workspaces.insert(ws.name, ws);
            if (ws.focused) {
              focused_workspace = ws;
            }
            break;
        }
      }
    }

    public signal void event(PayloadType type, string args);

    // Work space signals

    public signal void workspace_added(Workspace ws);
    public signal void workspace_removed(string name);
    public signal void workspace_focus(Workspace new_ws, Workspace old_ws);

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

    private async void handle_event(IpcReponse reply) {
      yield sync();
      if (focused_workspace != null) {
        print(focused_workspace.representation);
        print(focused_workspace.name);
        print(focused_workspace.id.to_string());
        print("\n");
      }
      switch (reply.type) {
        case PayloadType.MESSAGE_SUBSCRIBE:
          return;
        
        case PayloadType.EVENT_WORKSPACE:
          handle_workspace_event(reply.payload);
          break;
        
        case PayloadType.EVENT_WINDOW:
          // yield handle_client_event(reply.payload);
          break;

        default:
          break;
      }
      
      event(reply.type, reply.payload);
    }

    private void handle_workspace_event(string args) {
      notify_property("workspaces");
      
      var obj = Json.from_string(args).get_object();

      switch (obj.get_string_member("change")) {
        case "init": 
          var name = obj.get_object_member("current").get_string_member("name");
          workspace_added(get_workspace(name));
          break;

        case "empty":
          var name = obj.get_object_member("current").get_string_member("name");
          workspace_removed(name);
          break;

        case "focus":
          var new_name = obj.get_object_member("current").get_string_member("name");
          var old_name = obj.get_object_member("old").get_string_member("name");
          workspace_focus(get_workspace(new_name), get_workspace(old_name));
        
        default:
          break;
      }
    }

    private async void handle_client_event(string args) {

    }
  }
}
