namespace AstalGreet {
/**
 * Shorthand for creating a session, posting the password,
 * and starting the session with the given `cmd`
 * which is parsed with [func@GLib.shell_parse_argv].
 *
 * @param username User to login to
 * @param password Password of the user
 * @param cmd Command to start the session with
 */
public async void login(
    string username,
    string password,
    string cmd
) throws GLib.Error {
    yield login_with_env(username, password, cmd, {});
}

/**
 * Same as [func@AstalGreet.login] but allow for setting additonal env
 * in the form of `name=value` pairs.
 *
 * @param username User to login to
 * @param password Password of the user
 * @param cmd Command to start the session with
 * @param env Additonal env vars to set for the session
 */
public async void login_with_env(
    string username,
    string password,
    string cmd,
    string[] env
) throws GLib.Error {
    string[] argv;
    Shell.parse_argv(cmd, out argv);
    try {
        yield new CreateSession(username).send();
        yield new PostAuthMesssage(password).send();
        yield new StartSession(argv, env).send();
    } catch (GLib.Error err) {
        yield new CancelSession().send();
        throw err;
    }
}

/**
 * Base Request type.
 */
public abstract class Request : Object {
    protected abstract string type_name { get; }

    private string serialize() {
        var node = Json.gobject_serialize(this);
        var obj = node.get_object();
        obj.set_string_member("type", obj.get_string_member("type-name"));
        obj.remove_member("type-name");

        return Json.to_string(node, false);
    }

    private int bytes_to_int(Bytes bytes) {
        uint8[] data = (uint8[]) bytes.get_data();
        int value = 0;

        for (int i = 0; i < data.length; i++) {
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
        var payload = serialize();
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

        var response = (string)response_body.get_data();
        conn.close(null);

        var parser = new Json.Parser();
        parser.load_from_data(response);
        var obj = parser.get_root().get_object();
        var type = obj.get_string_member("type");

        switch (type) {
            case Success.TYPE: return new Success(obj);
            case Error.TYPE: return new Error(obj);
            case AuthMessage.TYPE: return new AuthMessage(obj);
            default: throw new IOError.NOT_FOUND("unknown response type");
        }
    }
}

/**
 * Creates a session and initiates a login attempted for the given user.
 * The session is ready to be started if a success is returned.
 */
public class CreateSession : Request {
    protected override string type_name { get { return "create_session"; } }
    public string username { get; set; }

    public CreateSession(string username) {
        Object(username: username);
    }
}

/**
 * Answers an authentication message.
 * If the message was informative (info, error),
 * then a response does not need to be set in this message.
 * The session is ready to be started if a success is returned.
 */
public class PostAuthMesssage : Request {
    protected override string type_name { get { return "post_auth_message_response"; } }
    public string response { get; set; }

    public PostAuthMesssage(string response) {
        Object(response: response);
    }
}

/**
 * Requests for the session to be started using the provided command line,
 * adding the supplied environment to that created by PAM.
 * The session will start after the greeter process terminates
 */
public class StartSession : Request {
    protected override string type_name { get { return "start_session"; } }
    public string[] cmd { get; set; }
    public string[] env { get; set; }

    public StartSession(string[] cmd, string[] env = {}) {
        Object(cmd: cmd, env: env);
    }
}

/**
 * Cancels the session that is currently under configuration.
 */
public class CancelSession : Request {
    internal override string type_name { get { return "cancel_session"; } }
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
 * Indicates that the request succeeded.
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
        ERROR;

        internal static Type from_string(string str) throws IOError {
            switch (str) {
                case "auth_error": return Type.AUTH_ERROR;
                case "error": return Type.ERROR;
                default: throw new IOError.FAILED(@"unknown error_type: $str");
            }
        }
    }

    public Type error_type { get; private set; }
    public string description { get; private set; }

    internal Error(Json.Object obj) throws IOError {
        error_type = Type.from_string(obj.get_string_member("error_type"));
        description = obj.get_string_member("description");
    }
}

/**
 * Indicates that the request succeeded.
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
        ERROR;

        internal static Type from_string(string str) throws IOError {
            switch (str) {
                case "visible": return VISIBLE;
                case "secret": return Type.SECRET;
                case "info": return Type.INFO;
                case "error": return Type.ERROR;
                default: throw new IOError.FAILED(@"unknown message_type: $str");
            }
        }
    }

    public Type message_type { get; private set; }
    public string message { get; private set; }

    internal AuthMessage(Json.Object obj) throws IOError {
        message_type = Type.from_string(obj.get_string_member("auth_message_type"));
        message = obj.get_string_member("auth_message");
    }
}
}
