namespace AstalBattery {
public PowerProfiles get_power_profiles() {
    return PowerProfiles.get_default();
}

public class PowerProfiles : Object {
    private static PowerProfiles instance;
    public static PowerProfiles get_default() {
        if (instance != null)
            return instance;

        instance = new PowerProfiles();
        return instance;
    }

    private IPowerProfiles proxy;
    private Properties props;

    public string[] actions { owned get { return proxy.actions; } }
    public string active_profile { owned get { return proxy.active_profile; } }
    public HashTable<string, Variant>[] active_profile_holds { owned get { return proxy.active_profile_holds; } }
    public string performance_degraded { owned get { return proxy.performance_degraded; } }
    public string performance_inhibited { owned get { return proxy.performance_inhibited; } }
    public HashTable<string, Variant>[] profiles { owned get { return proxy.profiles; } }
    public string version { owned get { return proxy.version; } }

    public signal uint profile_released (uint cookie);

    public uint hold_profile(string profile, string reason, string application_id) throws Error {
        return proxy.hold_profile(profile, reason, application_id);
    }

    public void release_profile(uint cookie) throws Error {
        proxy.release_profile(cookie);
    }

    construct {
        try {
            proxy = Bus.get_proxy_sync(
                BusType.SYSTEM,
                "org.freedesktop.UPower.PowerProfiles",
                "/org/freedesktop/UPower/PowerProfiles"
            );

            props = Bus.get_proxy_sync(
                BusType.SYSTEM,
                "org.freedesktop.UPower.PowerProfiles",
                "/org/freedesktop/UPower/PowerProfiles"
            );

            props.properties_changed.connect((iface, vardict) => {
                foreach (var key in vardict.get_keys()) {
                    var prop = pascal_to_kebab_case(key);
                    if (get_class().find_property(prop) != null)
                        notify_property(prop);
                }
            });

            proxy.profile_released.connect((cookie) => profile_released(cookie));
        } catch (Error error) {
            critical(error.message);
        }
    }
}
}
