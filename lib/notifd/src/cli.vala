using AstalNotifd;

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
        printerr(err.message);
        return 1;
    }

    if (help) {
        stdout.printf("Cli client for astal-notifd\n\n");
        stdout.printf("Usage:\n");
        stdout.printf("    %s [flags]\n\n", argv[0]);
        stdout.printf("Flags:\n");
        stdout.printf("    -h, --help           Print this help and exit\n");
        stdout.printf("    -v, --version        Print version number and exit\n");
        stdout.printf("    -l, --list           Print every notification and exit\n");
        stdout.printf("    -d, --daemonize      Watch for new notifications\n");
        stdout.printf("    -i, --invoke         Invoke a notification action\n");
        stdout.printf("    -c, --close          Close a notification by its id\n");
        stdout.printf("    -g, --get            Print a notification by its id\n");
        stdout.printf("    -t, --toggle-dnd     Toggle do not disturb\n");
        stdout.flush();
        return 0;
    }

    if (version) {
        print(VERSION);
        return 0;
    }

    Notifd.settings = new Settings("io.astal.notifd");

    if (list) {
        var n_list = Notifd.settings.get_value("notifications");
        print(Json.gvariant_serialize_data(n_list, null));
        return 0;
    }

    if (toggle_dnd) {
        var v = Notifd.settings.get_boolean("ignore-timeout");
        Notifd.settings.set_boolean("ignore-timeout", !v);
        return 0;
    }

    var notifd = Notifd.get_default();

    if (daemonize) {
        notifd.notified.connect((id) => {
            var n = notifd.get_notification(id).serialize();
            stdout.printf("%s\n", Json.gvariant_serialize_data(n, null));
            stdout.flush();
        });
        new MainLoop().run();
    }

    if (invoke != null) {
        if (!invoke.contains(":")) {
            print("invoke format needs to be <notif-id>:<action-id>");
            return 1;
        }

        var split = invoke.split(":");
        var n_id = int.parse(split[0]);
        var a_id = split[1];

        var n = notifd.get_notification(n_id);
        if (n != null) {
            n.invoke(a_id);
        } else {
            printerr(@"notification '$n_id' does not exist");
        }
    }

    if (close_n > 0) {
        var n = notifd.get_notification(close_n);
        if (n != null) {
            n.dismiss();

            // make sure to write state when daemon is not running
            var av = new VariantType.array(new VariantType("v"));
            var builder = new VariantBuilder(av);
            foreach (var notif in notifd.notifications) {
                if (!notif.transient) builder.add("v", notif.serialize());
            }
            Notifd.settings.set_value("notifications", builder.end());
        } else {
            printerr(@"notification '$close_n' does not exist");
        }
    }

    if (get_n > 0) {
        var n = notifd.get_notification(get_n);
        if (n != null) {
            print(Json.gvariant_serialize_data(n.serialize(), null));
        } else {
            printerr(@"notification '$get_n' does not exist");
        }
    }

    if (!daemonize && (invoke == null) && (close_n == 0) && (get_n == 0)) return 1;

    return 0;
}
