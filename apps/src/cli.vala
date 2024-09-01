static bool help;
static bool version;
static string search;
static string launch;
static bool json;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "search", 's', OptionFlags.NONE, OptionArg.STRING, ref search, null, null },
    { "launch", 'l', OptionFlags.NONE, OptionArg.STRING, ref launch, null, null },
    { "json", 'j', OptionFlags.NONE, OptionArg.NONE, ref json, null, null },
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
        print("    -s, --search      Sort by a search term\n");
        print("    -l, --launch      Launch an application\n");
        print("    -j, --json        Print list in json format\n");
        return 0;
    }

    if (version) {
        print(AstalApps.VERSION);
        return 0;
    }

    var apps = new AstalApps.Apps();

    if (launch != null) {
        apps.query(launch).first().data.launch();
        return 0;
    }

    if (json) {
        var b = new Json.Builder().begin_array();
        foreach (var app in apps.query(search))
            b.add_value(app.to_json());

        var generator = new Json.Generator();
        generator.set_root(b.end_array().get_root());
        stdout.printf(generator.to_data(null));
    } else {
        foreach (var app in apps.query(search))
            stdout.printf("%s\n", app.entry);
    }

    return 0;
}
