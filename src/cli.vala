static bool help;
static bool version;
static bool daemonize;
static bool list;
static string invoke;
static int close_n;
static int get_n;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "daemonize", 'd', OptionFlags.NONE, OptionArg.NONE, ref daemonize, null, null },
    { "list", 'l', OptionFlags.NONE, OptionArg.NONE, ref list, null, null },
    { "invoke", 'i', OptionFlags.NONE, OptionArg.STRING, ref invoke, null, null },
    { "close", 'c', OptionFlags.NONE, OptionArg.INT, ref close_n, null, null },
    { "get", 'g', OptionFlags.NONE, OptionArg.INT, ref get_n, null, null },
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
        print("    -h, --help        Print this help and exit\n");
        print("    -v, --version     Print version number and exit\n");
        print("    -l, --list        Print every notification and exit\n");
        print("    -d, --daemonize   Watch for new notifications\n");
        print("    -i, --invoke      Invoke a notification action\n");
        print("    -c, --close       Close a notification by its id\n");
        print("    -g, --get         Print a notification by its id\n");
        return 0;
    }

    var loop = new MainLoop();
    var notifd = new AstalNotifd.Notifd();

    if (version) {
        print(AstalNotifd.VERSION);
        return 0;
    }

    if (list) {
        var cache = Environment.get_user_cache_dir() + "/astal/notifd/notifications.json";
        if (FileUtils.test(cache, FileTest.EXISTS)) {
            try {
                uint8[] json;
                File.new_for_path(cache).load_contents(null, out json, null);
                stdout.printf("%s", (string)json);
            } catch (Error err) {
                stderr.printf("failed to load cache: %s", err.message);
            }
        }
        return 0;
    }

    if (daemonize) {
        notifd.notified.connect((id) => {
            stdout.printf("%s\n", notifd.get_notification_json(id));
            stdout.flush();
        });
    }

    if (invoke != null) {
        if (!invoke.contains(":")) {
            stderr.printf("invoke format needs to be <notif-id>:<action-id>");
            return 1;
        }

        var split = invoke.split(":");
        var n_id = int.parse(split[0]);
        var a_id = split[1];

        notifd.active.connect(() => {
            notifd.get_notification(n_id).invoke(a_id);
            loop.quit();
        });
    }

    if (close_n > 0) {
        notifd.active.connect(() => {
            notifd.get_notification(close_n).dismiss();
            loop.quit();
        });
    }

    if (get_n > 0) {
        notifd.active.connect(() => {
            stdout.printf("%s", notifd.get_notification(get_n).to_json_string());
            loop.quit();
        });
    }

    if (!daemonize && invoke == null && close_n == 0 && get_n == 0)
        return 1;

    loop.run();
    return 0;
}
