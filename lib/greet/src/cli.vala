using Quarrel;

static SpecialFlag help;
static SpecialFlag version;
static SpecialFlag interactive;
static StringArrayOpt env;

int err(string msg) {
    printerr(@"\x1b[1;31merror:\x1b[0m $msg\n");
    return 1;
}

async int main(string[] argv) {
    var cmd = new Command("astal-greet")
        .about("IPC client for greetd")
        .opt(help = new SpecialFlag("help", 'h', "Print help and exit"))
        .opt(version = new SpecialFlag("version", 'v', "Print version and exit"))
        .opt(interactive = new SpecialFlag("interactive", 'i', "Interactive login flow"))
        .opt(env = new StringArrayOpt(
            "env",
            'e',
            "Additional environment variables to set for the session"
        ) { name = "NAME=VALUE" })
        .required_arg("USERNAME", "User to log in as")
        .required_arg("PASSWORD", "Password of the user")
        .required_arg("CMD", "Command used to start the session");

    try {
        cmd.parse(argv);
    } catch (ParseError error) {
        err(error.message);
        return 1;
    }

    if (help.enabled) {
        print("%s\n", Quarrel.help(cmd));
        return 0;
    }

    if (version.enabled) {
        print("%s\n", AstalGreet.VERSION);
        return 0;
    }

    if (interactive.enabled) {
        interactive_loop(cmd);
        return 0;
    }

    try {
        yield AstalGreet.login_with_env(cmd.args[0], cmd.args[1], cmd.args[2], env.value);
    } catch (Error error) {
        err(error.message);
    }

    return 0;
}

void interactive_loop(Command cmd) {
    var loop = new MainLoop(null, false);
    var greeter = new AstalGreet.Greeter();

    greeter.visible_request.connect((message) => {
        stdout.printf("%s\n", message);
        greeter.post_auth(stdin.read_line());
    });

    // TODO: hide input
    greeter.secret_request.connect((message) => {
        stdout.printf("%s\n", message);
        greeter.post_auth(stdin.read_line());
    });

    greeter.info_message.connect((message) => {
        stdout.printf("%s\n", message);
    });

    greeter.error_message.connect((message) => {
        stdout.printf("%s\n", message);
    });

    greeter.cancelled.connect((error) => {
        stdout.printf("%s\n", error.description);
        stdout.printf("%s\n", "Username:");
        greeter.create_session(stdin.read_line());
    });

    greeter.authenticated.connect(() => {
        var parsing = true;

        while (parsing) {
            try {
                stdout.printf("%s\n", "Command:");
                var cmdline = stdin.read_line();
                string[] argv;
                Shell.parse_argv(cmdline, out argv);
                greeter.start_session.begin(argv, env.value, (_, res) => {
                    greeter.start_session.end(res);
                    loop.quit();
                });
                parsing = false;
            } catch (Error error) {
                stdout.printf("%s\n", error.message);
            }
        }
    });

    stdout.printf("%s\n", "Username:");
    greeter.create_session(stdin.read_line());

    loop.run();
}
