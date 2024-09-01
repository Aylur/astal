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
    print("%s\n", to_json(battery));

    if (monitor) {
        battery.notify.connect((prop) => {
            if (prop.get_name() == "percentage"
                || prop.get_name() == "state"
                || prop.get_name() == "icon-name"
                || prop.get_name() == "time-to-full"
                || prop.get_name() == "time-to-empty"
            ) {
                print("%s\n", to_json(battery));
            }
        });
        new GLib.MainLoop(null, false).run();
    }

    return 0;
}

private string to_json(AstalBattery.Device device) {
    string s = "unknown";
    if (device.state == AstalBattery.State.CHARGING)
        s = "charging";
    if (device.state == AstalBattery.State.DISCHARGING)
        s = "discharging";
    if (device.state == AstalBattery.State.FULLY_CHARGED)
        s = "fully_charged";

    var p = device.percentage;
    var i = device.icon_name;
    var r = device.state == AstalBattery.State.CHARGING
        ? device.time_to_full : device.time_to_empty;

    return "{ \"percentage\": %f, \"state\": \"%s\", \"icon_name\": \"%s\", \"time_remaining\": %f }".printf(p, s, i, r);
}
