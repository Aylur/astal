namespace AstalBattery {
[DBus (name = "org.freedesktop.DBus.Properties")]
public interface Properties : Object {
    public signal void properties_changed (
        string iface_name,
        HashTable<string, Variant> props,
        string[] invalidated_props
    );
}

[DBus (name = "org.freedesktop.UPower")]
interface IUPower : Object {
    public abstract string[] enumerate_devices() throws Error;
    public abstract string get_display_device() throws Error;
    public abstract string get_critical_action() throws Error;

    public signal void device_added(string object_path);
    public signal void device_removed(string object_path);

    public abstract string daemon_version { owned get; }
    public abstract bool on_battery { get; }
    public abstract bool lid_is_closed { get; }
    public abstract bool lis_is_present { get; }
}

[DBus (name = "org.freedesktop.UPower.KbdBacklight")]
interface IUPowerKdbBacklight : Object {
    public abstract int get_brightness() throws Error;
    public abstract int get_max_brightness() throws Error;
    public abstract void set_brightness(int value) throws Error;

    public signal void brightness_changed(int value);
    public signal void brightness_changed_with_source(int value, string source);
}

[DBus (name = "org.freedesktop.UPower.Device")]
public interface IUPowerDevice : Object {
    // public abstract void refresh() throws Error;
    // public abstract void get_history();
    // public abstract void get_statistics();

    // incompatible with gobject
    // public abstract uint type { get; }
    public abstract string native_path { owned get; }
    public abstract string vendor { owned get; }
    public abstract string model { owned get; }
    public abstract string serial { owned get; }
    public abstract uint64 update_time { get; }
    public abstract bool power_supply { get; }
    public abstract bool has_history { get; }
    public abstract bool has_statistics { get; }
    public abstract bool online { get; }
    public abstract double energy { get; }
    public abstract double energy_empty { get; }
    public abstract double energy_full { get; }
    public abstract double energy_full_design { get; }
    public abstract double energy_rate { get; }
    public abstract double voltage { get; }
    public abstract int charge_cycles { get; }
    public abstract double luminosity { get; }
    public abstract int64 time_to_empty { get; }
    public abstract int64 time_to_full { get; }
    public abstract double percentage { get; }
    public abstract double temperature { get; }
    public abstract bool is_present { get; }
    public abstract uint state { get; }
    public abstract bool is_rechargable { get; }
    public abstract double capacitiy { get; }
    public abstract uint technology { get; }
    public abstract uint warning_level { get; }
    public abstract uint battery_level { get; }
    public abstract string icon_name { owned get; }
}

[DBus (name = "org.freedesktop.UPower.PowerProfiles")]
public interface IPowerProfiles : Object {
    public abstract string[] actions { owned get; }
    public abstract string active_profile { owned get; }
    public abstract HashTable<string, Variant>[] active_profile_holds { owned get; }
    public abstract string performance_degraded { owned get; }
    public abstract string performance_inhibited { owned get; }
    public abstract HashTable<string, Variant>[] profiles { owned get; }
    public abstract string version { owned get; }

    public signal uint profile_released (uint cookie);

    public abstract uint hold_profile(string profile, string reason, string application_id) throws Error;
    public abstract void release_profile(uint cookie) throws Error;
}
}
