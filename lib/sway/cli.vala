static bool help;
static bool version;
static bool monitor;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "monitor", 'm', OptionFlags.NONE, OptionArg.NONE, ref monitor, null, null },
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
        return 0;
    }

    if (version) {
        print(AstalBattery.VERSION);
        return 0;
    }

    var battery = AstalBattery.get_default();
    print("%s\n", Json.gobject_to_data(battery, null));

    if (monitor) {
        battery.notify.connect(() => {
            print("%s\n", Json.gobject_to_data(battery, null));
        });
        new GLib.MainLoop(null, false).run();
    }

    return 0;
}
