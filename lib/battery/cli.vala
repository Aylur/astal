static bool help;
static bool version;
static bool monitor;
static bool pretty;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "monitor", 'm', OptionFlags.NONE, OptionArg.NONE, ref monitor, null, null },
    { "pretty", 'p', OptionFlags.NONE, OptionArg.NONE, ref pretty, null, null },
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
        print("    -m, --monitor     Monitor property changes\n");
        print("    -p, --pretty      Pretty print json output\n");
        return 0;
    }

    if (version) {
        print(AstalBattery.VERSION);
        return 0;
    }

    var battery = AstalBattery.get_default();
    print_state(battery);

    if (monitor) {
        battery.notify.connect(() => { print_state(battery); });
        new GLib.MainLoop(null, false).run();
    }

    return 0;
}

void print_state(AstalBattery.Device battery) {
    stdout.printf("%s\n", Json.to_string(Json.gobject_serialize(battery), pretty));
    stdout.flush();
}
