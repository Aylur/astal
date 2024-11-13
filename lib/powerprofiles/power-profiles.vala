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
    /** Gets the default singleton PowerProfiles instance. */
    return PowerProfiles.get_default();
}

/**
 * Client for  [[https://freedesktop-team.pages.debian.net/power-profiles-daemon/gdbus-org.freedesktop.UPower.PowerProfiles.html|PowerProfiles]].
 */
public class PowerProfiles : Object {
    private static PowerProfiles instance;

    /** Gets the default singleton PowerProfiles instance. */
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
                    if (key == "ActiveProfile")
                        notify_property("icon-name");
                }
            });
        } catch (Error error){
            critical(error.message);
        }
    }

    /**
     * The type of the currently active profile.
     * It might change automatically if a profile is held,
     * using the [method@AstalPowerProfiles.PowerProfiles.hold_profile] method.
     */
    public string active_profile {
        owned get { return proxy.active_profile; }
        set { proxy.active_profile = value; }
    }

    /**
     * Return a named icon based [property@AstalPowerProfiles.PowerProfiles:active_profile].
     */
    public string icon_name {
        owned get { return @"power-profile-$active_profile-symbolic"; }
    }

    /**
     * List of the "actions" implemented in the running daemon.
     * This can used to figure out whether particular functionality is available in the daemon.
     */
    public string[] actions {
        owned get { return proxy.actions.copy(); }
    }

    /**
     * List of dictionaries representing the current profile holds.
     */
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

    /**
     * This will be set if the performance power profile is running in degraded mode,
     * with the value being used to identify the reason for that degradation.
     * Possible values are:
     * - "lap-detected" (the computer is sitting on the user's lap)
     * - "high-operating-temperature" (the computer is close to overheating)
     * - "" (the empty string, if not performance is not degraded)
     */
    public string performance_degraded {
        owned get { return proxy.performance_degraded; }
    }

    /**
     * List of each profile.
     */
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

    /**
     * The version of the power-profiles-daemon software.
     */
    public string version {
        owned get { return proxy.version; }
    }

    /**
     * Emitted when the profile is released because
     * [property@AstalPowerProfiles.PowerProfiles:active_profile] was manually changed.
     * This will only be emitted to the process that originally called
     * [method@AstalPowerProfiles.PowerProfiles.hold_profile].
     */
    public signal void profile_released (uint cookie);

    /**
     * This forces the passed profile (either 'power-saver' or 'performance')
     * to be activated until either the caller quits,
     * [method@AstalPowerProfiles.PowerProfiles.release_profile] is called,
     * or the [property@AstalPowerProfiles.PowerProfiles:active_profile] is changed by the user.
     * When conflicting profiles are requested to be held,
     * the 'power-saver' profile will be activated in preference to the 'performance' profile.
     * Those holds will be automatically cancelled if the user manually switches to another profile,
     * and the [signal@AstalPowerProfiles.PowerProfiles::profile_released] signal will be emitted.
     */
    public int hold_profile(string profile, string reason, string application_id) {
        try {
            return (int)proxy.hold_profile(profile, reason, application_id);
        } catch (Error error) {
            critical(error.message);
            return -1;
        }
    }

    /**
     * This removes the hold that was set on a profile.
     */
    public void release_profile(uint cookie) {
        try {
            proxy.release_profile(cookie);
        } catch (Error error) {
            critical(error.message);
        }
    }
}

public struct Profile {
    /**
     * Will be one of:
     * - "power-saver" (battery saving profile)
     * - "balanced" (the default profile)
     * - "performance" (a profile that does not care about noise or battery consumption)
     */
    public string profile;
    public string cpu_driver;
    public string platform_driver;
    /**
     * Identifies the power-profiles-daemon backend code used to implement the profile.
     */
    public string driver;
}

public struct Hold {
    public string application_id;
    public string profile;
    public string reason;
}

private string kebab_case(string pascal_case) {
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
