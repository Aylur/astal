static bool help;
static bool version;
static bool test;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "test", 't', OptionFlags.NONE, OptionArg.NONE, ref test, null, null },
    // { "monitor", 'm', OptionFlags.NONE, OptionArg.NONE, ref monitor, null, null },
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

    if (test) {
        var loop = new MainLoop();
        AstalSway.get_default();
        loop.run();
        return 0;
    }

    if (version) {
        print(AstalSway.VERSION);
        return 0;
    }

    return 0;
}
