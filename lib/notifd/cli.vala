static bool help;
static bool version;
static bool daemonize;
static bool list;
static string invoke;
static int close_n;
static int get_n;
static bool toggle_dnd;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "daemonize", 'd', OptionFlags.NONE, OptionArg.NONE, ref daemonize, null, null },
    { "list", 'l', OptionFlags.NONE, OptionArg.NONE, ref list, null, null },
    { "invoke", 'i', OptionFlags.NONE, OptionArg.STRING, ref invoke, null, null },
    { "close", 'c', OptionFlags.NONE, OptionArg.INT, ref close_n, null, null },
    { "get", 'g', OptionFlags.NONE, OptionArg.INT, ref get_n, null, null },
    { "toggle-dnd", 't', OptionFlags.NONE, OptionArg.NONE, ref toggle_dnd, null, null },
    { null },
};

int main(string[] argv) {
    try {
        var opts = new OptionContext();
        opts.add_main_entries(options, null);
        opts.set_help_enabled(false);
        opts.set_ignore_unknown_options(false);
        opts.parse(ref argv);
    } catch (OptionError err) {
        printerr (err.message);
        return 1;
    }

    if (help) {
        print("Cli client for astal-notifd\n\n");
        print("Usage:\n");
        print("    %s [flags]\n\n", argv[0]);
        print("Flags:\n");
        print("    -h, --help           Print this help and exit\n");
        print("    -v, --version        Print version number and exit\n");
        print("    -l, --list           Print every notification and exit\n");
        print("    -d, --daemonize      Watch for new notifications\n");
        print("    -i, --invoke         Invoke a notification action\n");
        print("    -c, --close          Close a notification by its id\n");
        print("    -g, --get            Print a notification by its id\n");
        print("    -t, --toggle-dnd     Toggle do not disturb\n");
        return 0;
    }

    var notifd = AstalNotifd.get_default();

    if (version) {
        print(AstalNotifd.VERSION);
        return 0;
    }

    if (list) {
        var state = Environment.get_user_state_dir() + "/astal/notifd/notifications.json";
        if (FileUtils.test(state, FileTest.EXISTS)) {
            try {
                uint8[] json;
                File.new_for_path(state).load_contents(null, out json, null);

                var obj = Json.from_string((string)json);

                var list = obj.get_object().get_member("notifications");
                stdout.printf("%s\n", Json.to_string(list, true));
                return 0;
            } catch (Error err) {
                stderr.printf("failed to load cache: %s", err.message);
            }
        }
        stdout.printf("[]\n");
        return 0;
    }

    if (toggle_dnd) {
        notifd.dont_disturb = !notifd.dont_disturb;
        return 0;
    }

    if (daemonize) {
        notifd.notified.connect((id) => {
            stdout.printf("%s\n", notifd.get_notification_json(id));
            stdout.flush();
        });
        new MainLoop().run();
    }

    if (invoke != null) {
        if (!invoke.contains(":")) {
            stderr.printf("invoke format needs to be <notif-id>:<action-id>");
            return 1;
        }

        var split = invoke.split(":");
        var n_id = int.parse(split[0]);
        var a_id = split[1];

        notifd.get_notification(n_id).invoke(a_id);
    }

    if (close_n > 0) {
        notifd.get_notification(close_n).dismiss();
    }

    if (get_n > 0) {
        stdout.printf("%s", notifd.get_notification(get_n).to_json_string());
    }

    if (!daemonize && invoke == null && close_n == 0 && get_n == 0)
        return 1;

    return 0;
}
