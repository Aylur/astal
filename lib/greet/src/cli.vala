static bool help;
static bool version;
static string username;
static string password;
static string cmd;
[CCode(array_length = false, array_null_terminated = true)]
static string[] env;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "username", 'u', OptionFlags.NONE, OptionArg.STRING, ref username, null, null },
    { "password", 'p', OptionFlags.NONE, OptionArg.STRING, ref password, null, null },
    { "cmd", 'c', OptionFlags.NONE, OptionArg.STRING, ref cmd, null, null },
    { "env", 'e', OptionFlags.NONE, OptionArg.STRING_ARRAY, ref env, null, null },
    { null },
};

async int main(string[] argv) {
    try {
        var opts = new OptionContext();
        opts.add_main_entries(options, null);
        opts.set_help_enabled(false);
        opts.set_ignore_unknown_options(false);
        opts.parse(ref argv);
    } catch (OptionError err) {
        printerr(err.message);
        return 1;
    }

    if (help) {
        print("Usage:\n");
        print("    %s [flags]\n\n", argv[0]);
        print("Flags:\n");
        print("    -h, --help        Print this help and exit\n");
        print("    -v, --version     Print version number and exit\n");
        print("    -u, --username    User to login to\n");
        print("    -p, --password    Password of the user\n");
        print("    -c, --cmd         Command to start the session with\n");
        print("    -e, --env         Additional env vars to set for the session\n");
        return 0;
    }

    if (version) {
        printerr(AstalGreet.VERSION);
        return 0;
    }

    if (username == null) {
        printerr("missing username\n");
        return 1;
    }

    if (password == null) {
        printerr("missing password\n");
        return 1;
    }

    if (cmd == null) {
        printerr("missing cmd\n");
        return 1;
    }

    try {
        yield AstalGreet.login_with_env(username, password, cmd, env);
    } catch (Error err) {
        printerr(err.message);
        return 1;
    }

    return 0;
}
