namespace AstalPowerProfiles {
[DBus (name = "org.freedesktop.UPower.PowerProfiles")]
private interface IPowerProfiles : DBusProxy {
    public abstract string[] actions { owned get; }
    public abstract string active_profile { owned get; set; }
    public abstract HashTable<string, Variant>[] active_profile_holds { owned get; }
    public abstract string performance_degraded { owned get; }
    public abstract string performance_inhibited { owned get; }
    public abstract HashTable<string, Variant>[] profiles { owned get; }
    public abstract string version { owned get; }

    public signal void profile_released (uint cookie);

    public abstract uint hold_profile(string profile, string reason, string application_id) throws Error;
    public abstract void release_profile(uint cookie) throws Error;
}

public PowerProfiles get_default() {
    return PowerProfiles.get_default();
}

public class PowerProfiles : Object {
    private static PowerProfiles instance;
    public static PowerProfiles get_default() {
        if (instance == null)
            instance = new PowerProfiles();

        return instance;
    }

    private IPowerProfiles proxy;

    construct {
        try {
            proxy = Bus.get_proxy_sync(
                GLib.BusType.SYSTEM,
                "org.freedesktop.UPower.PowerProfiles",
                "/org/freedesktop/UPower/PowerProfiles"
            );

            proxy.profile_released.connect((cookie) => profile_released(cookie));
            proxy.g_properties_changed.connect((props) => {
                var map = (HashTable<string, Variant>)props;
                foreach (var key in map.get_keys()) {
                    notify_property(kebab_case(key));
                }
            });
        } catch (Error error){
            critical(error.message);
        }
    }

    public string active_profile {
        owned get { return proxy.active_profile; }
        set { proxy.active_profile = value; }
    }

    public string[] actions {
        owned get { return proxy.actions.copy(); }
    }

    public Hold[] active_profile_holds {
        owned get {
            Hold[] holds = new Hold[proxy.active_profile_holds.length];
            for (var i = 0; i < proxy.active_profile_holds.length; ++i) {
                var hold = proxy.active_profile_holds[i];
                holds[i] = Hold() {
                    application_id = hold.get("ApplicationId").get_string(),
                    profile = hold.get("Profile").get_string(),
                    reason = hold.get("Reason").get_string()
                };
            }
            return holds;
        }
    }

    public string performance_degraded {
        owned get { return proxy.performance_degraded; }
    }

    public string performance_inhibited {
        owned get { return proxy.performance_degraded; }
    }

    public Profile[] profiles {
        owned get {
            Profile[] profs = new Profile[proxy.profiles.length];
            for (var i = 0; i < proxy.profiles.length; ++i) {
                var prof = proxy.profiles[i];
                profs[i] = Profile() {
                    profile = prof.get("Profile").get_string(),
                    cpu_driver = prof.get("CpuDriver").get_string(),
                    platform_driver = prof.get("PlatformDriver").get_string(),
                    driver = prof.get("Driver").get_string()
                };
            }
            return profs;
        }
    }

    public string version {
        owned get { return proxy.version; }
    }

    public signal void profile_released (uint cookie);

    public int hold_profile(string profile, string reason, string application_id) {
        try {
            return (int)proxy.hold_profile(profile, reason, application_id);
        } catch (Error error) {
            critical(error.message);
            return -1;
        }
    }

    public void release_profile(uint cookie) {
        try {
            proxy.release_profile(cookie);
        } catch (Error error) {
            critical(error.message);
        }
    }

    public string to_json_string() {
        var acts = new Json.Builder().begin_array();
        foreach (var action in actions) {
            acts.add_string_value(action);
        }

        var active_holds = new Json.Builder().begin_array();
        foreach (var action in active_profile_holds) {
            active_holds.add_value(new Json.Builder()
                .begin_object()
                .set_member_name("application_id").add_string_value(action.application_id)
                .set_member_name("profile").add_string_value(action.profile)
                .set_member_name("reason").add_string_value(action.reason)
                .end_object()
                .get_root());
        }

        var profs = new Json.Builder().begin_array();
        foreach (var prof in profiles) {
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
            .set_member_name("active_profile").add_string_value(active_profile)
            .set_member_name("performance_degraded").add_string_value(performance_degraded)
            .set_member_name("performance_inhibited").add_string_value(performance_inhibited)
            .set_member_name("actions").add_value(acts.end_array().get_root())
            .set_member_name("active_profile_holds").add_value(active_holds.end_array().get_root())
            .set_member_name("profiles").add_value(profs.end_array().get_root())
            .end_object()
            .get_root(), false);
    }
}

public struct Profile {
    public string profile;
    public string cpu_driver;
    public string platform_driver;
    public string driver;
}

public struct Hold {
    public string application_id;
    public string profile;
    public string reason;
}

string kebab_case(string pascal_case) {
    StringBuilder kebab_case = new StringBuilder();

    for (int i = 0; i < pascal_case.length; i++) {
        char c = pascal_case[i];

        if (c >= 'A' && c <= 'Z') {
            if (i != 0) {
                kebab_case.append_c('-');
            }

            kebab_case.append_c((char)(c + 32));
        } else {
            kebab_case.append_c(c);
        }
    }

    return kebab_case.str;
}
}
