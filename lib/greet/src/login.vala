namespace AstalGreet {
    /**
     * Create a session, post the password, and start the session in one go.
     *
     * @param username User to log in as
     * @param password Password of the user
     * @param cmd Command used to start the session
     */
    public async void login(string username, string password, string cmd) throws GLib.Error {
        yield login_with_env(username, password, cmd, {});
    }

    /**
     * Same as [func@AstalGreet.login], but allows setting additional environment variables
     * in the form of `name=value` pairs.
     *
     * @param username User to log in as
     * @param password Password of the user
     * @param cmd Command used to start the session
     * @param env Additional environment variables to set for the session
     */
    public async void login_with_env(
        string username,
        string password,
        string cmd,
        string[] env
    ) throws GLib.Error {
        Response response;
        string[] argv;
        Shell.parse_argv(cmd, out argv);

        try {
            // first step
            response = yield new CreateSession(username).send();
            if (response is Error) throw_response(response);

            if (response is Success) {
                response = yield new StartSession(argv, env).send();
                if (!(response is Success)) throw_response(response);
                return;
            }

            // auth step
            response = yield new PostAuthMessage(password).send();
            if (!(response is Success)) throw_response(response);

            // last step
            response = yield new StartSession(argv, env).send();
            if (!(response is Success)) throw_response(response);
        } catch (GLib.Error err) {
            yield new CancelSession().send();
            throw err;
        }
    }

    /**
     * An error that may occur during [func@AstalGreet.login].
     */
    public errordomain LoginError {
        /** Authentication error. See [enum@AstalGreet.ErrorType.AUTH_ERROR]. */
        AUTH_ERROR,
        /** General error. See [enum@AstalGreet.ErrorType.ERROR]. */
        ERROR,
        /** Unexpected [class@AstalGreet.AuthMessage]. */
        UNEXPECTED_AUTH,
    }

    private void throw_response(Response response) throws LoginError {
        if (response is Error) {
            var error = response as Error;
            switch (error.error_type) {
                case Error.Type.ERROR:
                    throw new LoginError.ERROR(error.description);
                case Error.Type.AUTH_ERROR:
                    throw new LoginError.AUTH_ERROR(error.description);
            }
        }

        if (response is AuthMessage) {
            var auth = response as AuthMessage;
            throw new LoginError.UNEXPECTED_AUTH(auth.message);
        }
    }
}
