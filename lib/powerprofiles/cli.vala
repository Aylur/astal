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

        stdout.printf("%s\n", to_json_string(profiles));
        stdout.flush();

        profiles.notify.connect(() => {
            stdout.printf("%s\n", to_json_string(profiles));
            stdout.flush();
        });

        profiles.profile_released.connect(() => {
            stdout.printf("%s\n", to_json_string(profiles));
            stdout.flush();
        });

        loop.run();
    }

    if (set == null && !daemonize) {
        stdout.printf("%s\n", to_json_string(profiles));
    }

    return 0;
}

string to_json_string(AstalPowerProfiles.PowerProfiles profiles) {
    var acts = new Json.Builder().begin_array();
    foreach (var action in profiles.actions) {
        acts.add_string_value(action);
    }

    var active_holds = new Json.Builder().begin_array();
    foreach (var action in profiles.active_profile_holds) {
        active_holds.add_value(new Json.Builder()
            .begin_object()
            .set_member_name("application_id").add_string_value(action.application_id)
            .set_member_name("profile").add_string_value(action.profile)
            .set_member_name("reason").add_string_value(action.reason)
            .end_object()
            .get_root());
    }

    var profs = new Json.Builder().begin_array();
    foreach (var prof in profiles.profiles) {
        profs.add_value(new Json.Builder()
            .begin_object()
            .set_member_name("profie").add_string_value(prof.profile)
            .set_member_name("driver").add_string_value(prof.driver)
            .set_member_name("cpu_driver").add_string_value(prof.cpu_driver)
            .set_member_name("platform_driver").add_string_value(prof.platform_driver)
            .end_object()
            .get_root());
    }

    return Json.to_string(new Json.Builder()
        .begin_object()
        .set_member_name("active_profile").add_string_value(profiles.active_profile)
        .set_member_name("icon_name").add_string_value(profiles.icon_name)
        .set_member_name("performance_degraded").add_string_value(profiles.performance_degraded)
        .set_member_name("actions").add_value(acts.end_array().get_root())
        .set_member_name("active_profile_holds").add_value(active_holds.end_array().get_root())
        .set_member_name("profiles").add_value(profs.end_array().get_root())
        .end_object()
        .get_root(), false);
}
