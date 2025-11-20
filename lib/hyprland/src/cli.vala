static bool help;
static bool version;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
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
        print("Usage:\n");
        print("    %s [flags]\n\n", argv[0]);
        print("Flags:\n");
        print("    -h, --help        Print this help and exit\n");
        print("    -v, --version     Print version number and exit\n");
        return 0;
    }

    if (version) {
        print(AstalHyprland.VERSION);
        return 0;
    }

    AstalHyprland.Hyprland.get_default().event.connect((event, args) => {
        print("{ event: \"%s\", payload: \"%s\" }\n", event, args);
    });

    new MainLoop(null, false).run();
    return 0;
}
