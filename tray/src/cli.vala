static bool version;
static bool daemonize;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, "Print version number", null },
    { "daemonize", 'd', OptionFlags.NONE, OptionArg.NONE, ref daemonize, "Monitor the systemtray", null },
    { null },
};

int main(string[] argv) {
    try {
        var opts = new OptionContext();
        opts.add_main_entries(options, null);
        opts.set_help_enabled(true);
        opts.set_ignore_unknown_options(false);
        opts.parse(ref argv);
    } catch (OptionError err) {
        printerr (err.message);
        return 1;
    }

    if (version) {
        print(AstalTray.VERSION);
        return 0;
    }

    if (daemonize) {
        var loop = new MainLoop();
        var tray = new AstalTray.Tray();

        tray.item_added.connect((id) => {
            AstalTray.TrayItem item = tray.get_item(id);

            stdout.printf("{\"event\":\"item_added\",\"id\":\"%s\",\"item\":%s}\n",
                id, item.to_json_string());
            stdout.flush();

            item.changed.connect(() => {
                stdout.printf("{\"event\":\"item_changed\",\"id\":\"%s\",\"item\":%s}\n",
                    id, item.to_json_string());
                stdout.flush();
            });
        });

        tray.item_removed.connect((id) => {
            stdout.printf("{\"event\":\"item_removed\",\"id\":\"%s\"}\n", id);
            stdout.flush();
        });

        loop.run();
    }

    return 0;
}
