namespace AstalGreet {
/**
 * Base Request type.
 */
public abstract class Request : Object {
    protected abstract Json.Node payload();

    private int bytes_to_int(Bytes bytes) {
        // native endian length
        uint8[] data = (uint8[])bytes.get_data();
        int value = 0;

        for (int i = data.length - 1; i >= 0; i--) {
            value = (value << 8) | data[i];
        }

        return value;
    }

    /**
     * Send this request to greetd.
     */
    public async Response send() throws GLib.Error {
        var sock = Environment.get_variable("GREETD_SOCK");
        if (sock == null) {
            throw new IOError.NOT_FOUND("greetd socket not found");
        }

        var addr = new UnixSocketAddress(sock);
        var socket = new SocketClient();
        var conn = socket.connect(addr);
        var payload = Json.to_string(payload(), false);
        var ostream = new DataOutputStream(conn.get_output_stream()) {
            byte_order = DataStreamByteOrder.HOST_ENDIAN,
        };

        ostream.put_int32(payload.length, null);
        ostream.put_string(payload, null);
        ostream.close(null);

        var istream = conn.get_input_stream();

        var response_head = yield istream.read_bytes_async(4, Priority.DEFAULT, null);
        var response_length = bytes_to_int(response_head);
        var response_body = yield istream.read_bytes_async(response_length, Priority.DEFAULT, null);

        // get_data() has no NUL terminator
        var response = (string)response_body.get_data();
        conn.close(null);

        var parser = new Json.Parser();
        parser.load_from_data(response, response_length);
        var obj = parser.get_root().get_object();
        var type = obj.get_string_member("type");

        switch (type) {
            case Success.TYPE: return new Success(obj);
            case Error.TYPE: return new Error(obj);
            case AuthMessage.TYPE: return new AuthMessage(obj);
            default: assert_not_reached();
        }
    }
}

/**
 * Creates a session and initiates a login attempt for the given user.
 * The session is ready to be started if a success is returned.
 */
public class CreateSession : Request {
    public string username { get; set; }

    public CreateSession(string username) {
        Object(username: username);
    }

    protected override Json.Node payload() {
        return new Json.Builder().begin_object()
            .set_member_name("type").add_string_value("create_session")
            .set_member_name("username").add_string_value(username)
            .end_object().get_root();
    }
}

/**
 * Responds to an authentication message.
 * If the message is informational (info or error),
 * then this message does not need a response.
 * The session is ready to be started if a success is returned.
 */
public class PostAuthMessage : Request {
    public string? response { get; set; }

    public PostAuthMessage(string? response) {
        Object(response: response);
    }

    protected override Json.Node payload() {
        var builder = new Json.Builder().begin_object()
            .set_member_name("type").add_string_value("post_auth_message_response");

        if (response != null) {
            builder.set_member_name("response").add_string_value(response);
        }

        return builder.end_object().get_root();
    }
}

/**
 * Requests that the session be started using the provided command line,
 * adding the supplied environment to the environment created by PAM.
 * The session will start after the greeter process terminates.
 */
public class StartSession : Request {
    public string[] cmd { get; set; }
    public string[] env { get; set; }

    public StartSession(string[] cmd, string[] env = {}) {
        Object(cmd: cmd, env: env);
    }

    protected override Json.Node payload() {
        var _cmd = new Json.Builder().begin_array();
        foreach (var value in cmd) {
            _cmd.add_string_value(value);
        }

        var _env = new Json.Builder().begin_array();
        foreach (var value in env) {
            _env.add_string_value(value);
        }

        return new Json.Builder().begin_object()
            .set_member_name("type").add_string_value("start_session")
            .set_member_name("cmd").add_value(_cmd.end_array().get_root())
            .set_member_name("env").add_value(_env.end_array().get_root())
            .end_object().get_root();
    }
}

/**
 * Cancels the session that is currently under configuration.
 */
public class CancelSession : Request {
    protected override Json.Node payload() {
        return new Json.Builder().begin_object()
            .set_member_name("type").add_string_value("cancel_session")
            .end_object().get_root();
    }
}

/**
 * Base Response type.
 */
public abstract class Response : Object {
    // nothing to do
}

/**
 * Indicates that the request succeeded.
 */
public class Success : Response {
    internal const string TYPE = "success";

    internal Success(Json.Object obj) {
        // nothing to do
    }
}

/**
 * Indicates that the request failed.
 */
public class Error : Response {
    internal const string TYPE = "error";

    public enum Type {
        /**
         * Indicates that authentication failed.
         * This is not a fatal error, and is likely caused by incorrect credentials.
         */
        AUTH_ERROR,
        /**
         * A general error.
         * See the error description for more information.
         */
        ERROR,
    }

    public Type error_type { get; private set; }
    public string description { get; private set; }

    internal Error(Json.Object obj) {
        description = obj.get_string_member("description");

        switch (obj.get_string_member("error_type")) {
            case "auth_error": error_type = AUTH_ERROR; break;
            case "error": error_type = Type.ERROR; break;
            default: assert_not_reached();
        }
    }
}

/**
 * Indicates that the request returned an authentication message.
 */
public class AuthMessage : Response {
    internal const string TYPE = "auth_message";

    public enum Type {
        /**
         * Indicates that input from the user should be
         * visible when they answer this question.
         */
        VISIBLE,
        /**
         * Indicates that input from the user should be
         * considered secret when they answer this question.
         */
        SECRET,
        /**
         * Indicates that this message is informative, not a question.
         */
        INFO,
        /**
         * Indicates that this message is an error, not a question.
         */
        ERROR,
    }

    public Type message_type { get; private set; }
    public string message { get; private set; }

    internal AuthMessage(Json.Object obj) {
        message = obj.get_string_member("auth_message");

        switch (obj.get_string_member("auth_message_type")) {
            case "visible": message_type = VISIBLE; break;
            case "secret": message_type = SECRET; break;
            case "info": message_type = INFO; break;
            case "error": message_type = ERROR; break;
            default: assert_not_reached();
        }
    }
}
}
