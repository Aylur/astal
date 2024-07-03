static bool help;
static bool version;
static bool daemonize;
static bool list;
static string set;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "daemonize", 'd', OptionFlags.NONE, OptionArg.NONE, ref daemonize, null, null },
    { "list", 'l', OptionFlags.NONE, OptionArg.NONE, ref list, null, null },
    { "set", 's', OptionFlags.NONE, OptionArg.STRING, ref set, null, null },
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
        print("Usage:\n");
        print("    %s [flags]\n\n", argv[0]);
        print("Flags:\n");
        print("    -h, --help        Print this help and exit\n");
        print("    -v, --version     Print version number and exit\n");
        print("    -d, --daemonize   Monitor for changes\n");
        print("    -l, --list        List available profiles\n");
        return 0;
    }

    if (version) {
        print(AstalPowerProfiles.VERSION);
        return 0;
    }

    var profiles = AstalPowerProfiles.get_default();
    if (set != null) {
        profiles.active_profile = set;
    }

    else if (list) {
        foreach (var p in profiles.profiles) {
            print("%s\n", p.profile);
        }
        return 0;
    }

    if (daemonize) {
        var loop = new MainLoop();

        stdout.printf("%s\n", profiles.to_json_string());
        stdout.flush();

        profiles.notify.connect(() => {
            stdout.printf("%s\n", profiles.to_json_string());
            stdout.flush();
        });

        profiles.profile_released.connect(() => {
            stdout.printf("%s\n", profiles.to_json_string());
            stdout.flush();
        });

        loop.run();
    }

    if (set == null && !daemonize) {
        stdout.printf("%s\n", profiles.to_json_string());
    }

    return 0;
}
