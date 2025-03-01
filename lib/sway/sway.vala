namespace AstalSway {
    public Sway get_default() {
        return Sway.get_default();
    }


  public class Sway : Object {
    private static Sway _instance;
    private Ipc ipc;
    private SocketConnection subscribe_socket;
    
    public Workspace focused_workspace;
  

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

    public async void subscribe() {
      if (subscribe_socket != null) {
        return;
      }

      subscribe_socket = ipc.connection();
      ipc.send(subscribe_socket.output_stream, PayloadType.MESSAGE_SUBSCRIBE, "[ \"workspace\", \"window\" ]");
      while (true) {
        var result = yield ipc.receive_async(subscribe_socket.input_stream);
        print(result.payload);
      }
    }
  }
}
