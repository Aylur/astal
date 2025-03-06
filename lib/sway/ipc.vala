namespace AstalSway {
  private const string IPC_MAGIC = "i3-ipc";

  public enum PayloadType {
    MESSAGE_RUN_COMMAND = 0,
    MESSAGE_GET_WORKSPACES = 1,
    MESSAGE_SUBSCRIBE = 2,
    MESSAGE_GET_OUTPUTS = 3,
    MESSAGE_GET_TREE = 4,
    MESSAGE_GET_MARKS = 5,
    MESSAGE_GET_BAR_CONFIG = 6,
    MESSAGE_GET_VERSION = 7,
    MESSAGE_GET_BINDING_NODES = 8,
    MESSAGE_GET_CONFIG = 9,
    MESSAGE_SEND_TICK = 10,
    MESSAGE_SYNC = 11,
    MESSAGE_GET_BINDING_STATE = 12,
    MESSAGE_GET_INPUTS = 100,
    MESSAGE_GET_SEATS = 101,
    EVENT_WORKSPACE = 0x80000000,
    EVENT_OUTPUT = 0x80000001,
    EVENT_MODE = 0x80000002,
    EVENT_WINDOW = 0x80000003,
    EVENT_BARCONFIG_UPDATE = 0x80000004,
    EVENT_BINDING = 0x80000005,
    EVENT_SHUTDOWN = 0x80000006,
    EVENT_TICK = 0x80000007,
    EVENT_BAR_STATE_UPDATE = 0x80000014,
    EVENT_INPUT = 0x80000015,
  }

  private struct IpcReponse {
    public PayloadType type;
    public string payload;
  }

  
  // Basic interface to send and receive data through Sway IPjC
  private class Ipc : Object {
    private string SWAYSOCK = GLib.Environment.get_variable("SWAYSOCK");

    internal void init() throws Error {
      SWAYSOCK = GLib.Environment.get_variable("SWAYSOCK");
      
      if (SWAYSOCK == null) {
        critical("Unable to detect Sway");
        return;
      }
    }
    
    internal SocketConnection? connection() {
      try {
          SocketConnection socket = new SocketClient().connect(new UnixSocketAddress(SWAYSOCK), null);
          return socket;
      } catch (Error err) {
          critical(err.message);
          return null;
      }
    }

    internal void send(OutputStream stream, PayloadType type, string payload) {
      Array<uint8> message = new Array<uint8> ();

      uint8[] magic_str = IPC_MAGIC.data;
      uint32 pl_length = (uint32) payload.length; 
      uint32 pl_type = (uint32) type;
      uint8[] pl_data = payload.data;

      message.append_vals(magic_str, magic_str.length);
      message.append_vals((uint8 *)&pl_length, 4);
      message.append_vals((uint8 *)&pl_type, 4);
      message.append_vals(pl_data, pl_data.length);
      
      stream.write(message.data);
    }

    internal IpcReponse? receive(InputStream stream) {
      var header = stream.read_bytes(14);
        uint8 data[14] = header.get_data();
        if (data == null) {
            return null;
        }
        uint32 pl_length = *(uint32 *)&data[IPC_MAGIC.length];
        PayloadType pl_type = *(uint32 *)&data[IPC_MAGIC.length+4];
         
        var payload = stream.read_bytes(pl_length);
        
        var result = payload.get_data();
        result += '\0';
        
        return {pl_type, (string)result};

    }

    internal async IpcReponse? receive_async(InputStream stream) {
      var header = yield stream.read_bytes_async(14, Priority.DEFAULT, null);
        uint8 data[14] = header.get_data();
        if (data == null) {
            return null;
        }
        uint32 pl_length = *(uint32 *)&data[IPC_MAGIC.length];
        PayloadType pl_type = *(uint32 *)&data[IPC_MAGIC.length+4];
         
        var payload = yield stream.read_bytes_async(pl_length, Priority.DEFAULT, null);
        
        var result = payload.get_data();
        result += '\0';

        return {pl_type, (string)result};
    }

    public string message(PayloadType type, string payload) {
      SocketConnection conn = connection();
      if (conn == null) {
        return "";
      }
      
      send(conn.output_stream, type, payload);
      var result = receive(conn.input_stream);
      conn.close(null);
     
      return result.payload;
    }
    
    public async string message_async(PayloadType type, string payload) {
      SocketConnection conn = connection();
      if (conn == null) {
        return "";
      }
      
      send(conn.output_stream, type, payload);
      var result = yield receive_async(conn.input_stream);
      conn.close(null);
     
      return result.payload;
    }

  }
}
