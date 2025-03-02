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

    public async void sync() {
      var str = yield message_async(PayloadType.MESSAGE_GET_WORKSPACES, "");
      var arr = Json.from_string(str).get_array();
      foreach (var obj in arr.get_elements()) {
        var name = obj.get_object().get_string_member("name");
        var ws = get_workspace(name);
        if (ws != null)
          ws.sync(obj.get_object());

      }
    }

    public signal void event(PayloadType type, string args);

    // Work space signals

    public signal void workspace_added(Workspace ws);
    public signal void workspace_removed(string name);

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
      switch (reply.type) {
        case PayloadType.MESSAGE_SUBSCRIBE:
          return;
        
        case PayloadType.EVENT_WORKSPACE:
          yield handle_workspace_event(reply.payload);
          break;
        
        case PayloadType.EVENT_WINDOW:
          // yield handle_client_event(reply.payload);
          break;

        default:
          break;
      }
      
      event(reply.type, reply.payload);
    }

    private async void handle_workspace_event(string args) {
      var data = Json.from_string(args).get_object();

      switch (data.get_string_member("change")) {
        case "init": 
          var obj = data.get_object_member("current");
          var ws = new Workspace();
          _workspaces.insert(obj.get_string_member("name"), ws);
          yield sync();
          workspace_added(ws);
          break;

        case "empty":
          var obj = data.get_object_member("current");
          var name = obj.get_string_member("name");
          _workspaces.remove(name);
          workspace_removed(name);
          var stuff = yield message_async(PayloadType.MESSAGE_GET_TREE, "");
          Node.build(Json.from_string(stuff).get_object());
          break;
        
        default:
          break;
      }
    }

    private async void handle_client_event(string args) {

    }
  }
}
