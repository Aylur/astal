private static bool version;
private static bool help;
private static bool list;
private static bool quit;
private static bool inspector;
private static string? toggle_window;
private static string? instance_name;

private const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "list", 'l', OptionFlags.NONE, OptionArg.NONE, ref list, null, null },
    { "quit", 'q', OptionFlags.NONE, OptionArg.NONE, ref quit, null, null },
    { "quit", 'q', OptionFlags.NONE, OptionArg.NONE, ref quit, null, null },
    { "inspector", 'I', OptionFlags.NONE, OptionArg.NONE, ref inspector, null, null },
    { "toggle-window", 't', OptionFlags.NONE, OptionArg.STRING, ref toggle_window, null, null },
    { "instance", 'i', OptionFlags.NONE, OptionArg.STRING, ref instance_name, null, null },
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
        print("Client for Astal.Application instances\n\n");
        print("Usage:\n");
        print("    %s [flags] message\n\n", argv[0]);
        print("Flags:\n");
        print("    -h, --help            Print this help and exit\n");
        print("    -v, --version         Print version number and exit\n");
        print("    -l, --list            List running Astal instances and exit\n");
        print("    -q, --quit            Quit an Astal.Application instance\n");
        print("    -i, --instance        Instance name of the Astal instance\n");
        print("    -I, --inspector       Open up Gtk debug tool\n");
        print("    -t, --toggle-window   Show or hide a window\n");
        return 0;
    }

    if (version) {
        print(AstalIO.VERSION);
        return 0;
    }

    if (instance_name == null)
        instance_name = "astal";

    if (list) {
        foreach (var name in AstalIO.get_instances())
            stdout.printf("%s\n", name);

        return 0;
    }

    if (quit) {
        AstalIO.quit_instance(instance_name);
        return 0;
    }

    if (inspector) {
        AstalIO.open_inspector(instance_name);
        return 0;
    }

    if (toggle_window != null) {
        AstalIO.toggle_window_by_name(instance_name, toggle_window);
        return 0;
    }

    var request = "";
    for (var i = 1; i < argv.length; ++i) {
        request = request.concat(" ", argv[i]);
    }

    var reply = AstalIO.send_message(instance_name, request);
    print("%s\n", reply);

    return 0;
}
