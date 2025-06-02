namespace AstalSway {
private const string IPC_MAGIC = "i3-ipc";

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
    
    internal SocketConnection connection() throws Error {
        return new SocketClient().connect(new UnixSocketAddress(SWAYSOCK), null);
    }

    internal void send(OutputStream stream, PayloadType type, string payload) throws Error {
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
        try {
            var header = stream.read_bytes(14);
            uint8 data[14] = header.get_data();
            uint32 pl_length = *(uint32 *)&data[IPC_MAGIC.length];
            PayloadType pl_type = *(uint32 *)&data[IPC_MAGIC.length+4];
             
            var payload = stream.read_bytes(pl_length);
            
            var result = payload.get_data();
            result += '\0';
            
            return {pl_type, (string)result};
        } catch (Error err) {
            critical("could not receive message: %s", err.message);
            return null;
        }
    }

    internal async IpcReponse? receive_async(InputStream stream) {
        try {
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
        } catch (Error err) {
            critical("could not receive message: %s", err.message);
            return null;
        }
    }

    public string message(PayloadType type, string payload) {
        try {
            SocketConnection conn = connection();
            if (conn == null) {
                return "";
            }
            
            send(conn.output_stream, type, payload);
            var result = receive(conn.input_stream);
            conn.close(null);
            
            if (result == null) {
                return "";
            }
         
            return result.payload;
        } catch (Error err) {
           critical("message failed: %s", err.message);
           return "";
        }
    }
    
    public async string message_async(PayloadType type, string payload) {
        try {
            SocketConnection conn = connection();
            if (conn == null) {
                return "";
            }
            
            send(conn.output_stream, type, payload);
            var result = yield receive_async(conn.input_stream);
            conn.close(null);
            
            if (result == null) {
                return "";
            }
         
            return result.payload;
        } catch (Error err) {
           critical("message failed: %s", err.message);
           return "";
        }
    }
}
}
