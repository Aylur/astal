[DBus (name = "org.freedesktop.UPower")]
private interface AstalBattery.IUPower : DBusProxy {
    public abstract ObjectPath[] enumerate_devices() throws Error;
    public abstract ObjectPath get_display_device() throws Error;
    public abstract string get_critical_action() throws Error;

    public signal void device_added(ObjectPath object_path);
    public signal void device_removed(ObjectPath object_path);

    public abstract string daemon_version { owned get; }
    public abstract bool on_battery { get; }
    public abstract bool lid_is_closed { get; }
    public abstract bool lis_is_present { get; }
}

[DBus (name = "org.freedesktop.UPower.Device")]
private interface AstalBattery.IUPowerDevice : DBusProxy {
    // public abstract HistoryDataPoint[] get_history (string type, uint32 timespan, uint32 resolution) throws GLib.Error;
    // public abstract StatisticsDataPoint[] get_statistics (string type) throws GLib.Error;
    // public abstract void refresh () throws GLib.Error;

    public abstract uint Type { get; }
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
    public abstract int32 charge_cycles { get; }
    public abstract double luminosity { get; }
    public abstract int64 time_to_empty { get; }
    public abstract int64 time_to_full { get; }
    public abstract double percentage { get; }
    public abstract double temperature { get; }
    public abstract bool is_present { get; }
    public abstract uint state { get; }
    public abstract bool is_rechargable { get; }
    public abstract double capacity { get; }
    public abstract uint technology { get; }
    public abstract uint32 warning_level { get; }
    public abstract uint32 battery_level { get; }
    public abstract string icon_name { owned get; }
}

// private struct AstalBattery.HistoryDataPoint {
//     uint32 time;
//     double value;
//     uint32 state;
// }
// 
// private struct AstalBattery.StatisticsDataPoint {
//     double value;
//     double accuracy;
// }
