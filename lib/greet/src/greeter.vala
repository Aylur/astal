/**
 * Object that allows login flows to be implemented using signals.
 */
public class AstalGreet.Greeter : Object {
    private Step step = CREATE;

    enum Step {
        CREATE,
        POST,
        START,
    }

    /**
     * Respond with [method@AstalGreet.Greeter.post_auth].
     * Emitted when input is required from the user.
     * The response is not secret and can be visible.
     */
    public signal void visible_request(string message);

    /**
     * Respond with [method@AstalGreet.Greeter.post_auth].
     * Emitted when input is required from the user.
     * The response is secret and should be invisible.
     */
    public signal void secret_request(string message);

    /**
     * Information that should be shown to the user.
     */
    public signal void info_message(string message);

    /**
     * Error message that should be shown to the user.
     */
    public signal void error_message(string message);

    /**
     * Emitted when the session is cancelled due to an error.
     * The login can be restarted with [method@AstalGreet.Greeter.create_session].
     */
    public signal void cancelled(Error error);

    /**
     * Emitted on successful authentication to indicate that the session can be
     * started using [method@AstalGreet.Greeter.start_session].
     */
    public signal void authenticated();

    /**
     * Create a session and start the authentication flow.
     */
    public void create_session(string username)
        requires (step == CREATE) {
        new CreateSession(username).send.begin(end_send);
    }

    /**
     * Respond to [signal@AstalGreet.Greeter::visible_request] or [signal@AstalGreet.Greeter::secret_request].
     */
    public void post_auth(string? response)
        requires (step == POST) {
        new PostAuthMessage(response).send.begin(end_send);
    }

    /**
     * Start the session after [signal@AstalGreet.Greeter::authenticated] is emitted.
     * The greeter process must terminate after this method yields in order for the session to start.
     */
    public async void start_session(string[] cmd, string[] env)
        requires (step == START) {
        try {
            yield new StartSession(cmd, env).send();
        } catch (GLib.Error e) {
            step = CREATE;
            critical(e.message);
        }
    }

    private void end_send(Object? object, AsyncResult result) {
        Request req = object as Request;
        Response res;

        try {
            res = req.send.end(result);
        } catch (GLib.Error e) {
            critical(e.message);
            return;
        }

        if (res is Error) {
            step = CREATE;
            new CancelSession().send.begin(() => {
                cancelled(res as Error);
            });
        }

        if (res is AuthMessage) {
            step = POST;
            switch (res.message_type) {
                case VISIBLE:
                    visible_request(res.message);
                    break;
                case SECRET:
                    secret_request(res.message);
                    break;
                case ERROR:
                    error_message(res.message);
                    post_auth(null);
                    break;
                case INFO:
                    info_message(res.message);
                    post_auth(null);
                    break;
            }
        }

        if (res is Success) {
            step = START;
            authenticated();
        }
    }
}
