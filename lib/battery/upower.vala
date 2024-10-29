/**
 * Client for the UPower [[https://upower.freedesktop.org/docs/UPower.html|dbus interface]].
 */
public class AstalBattery.UPower : Object {
    private IUPower proxy;
    private HashTable<string, Device> _devices =
        new HashTable<string, Device>(str_hash, str_equal);

    /** List of UPower devices. */
    public List<weak Device> devices {
        owned get { return _devices.get_values(); }
    }

    /** Emitted when a new device is connected. */
    public signal void device_added(Device device);

    /** Emitted a new device is disconnected. */
    public signal void device_removed(Device device);

    /** A composite device that represents the battery status. */
    public Device display_device { owned get { return Device.get_default(); }}

    public string daemon_version { owned get { return proxy.daemon_version; } }

    /** Indicates whether the system is running on battery power. */
    public bool on_battery { get { return proxy.on_battery; } }

    /** Indicates if the laptop lid is closed where the display cannot be seen. */
    public bool lid_is_closed { get { return proxy.lid_is_closed; } }

    /** Indicates if the system has a lid device. */
    public bool lis_is_present { get { return proxy.lid_is_closed; } }

    /**
     * When the system's power supply is critical (critically low batteries or UPS),
     * the system will take this action.
     */
    public string critical_action {
        owned get {
            try {
                return proxy.get_critical_action();
            } catch (Error error) {
                critical(error.message);
                return "";
            }
        }
    }

    construct {
        try {
            proxy = Bus.get_proxy_sync(
                BusType.SYSTEM,
                "org.freedesktop.UPower",
                "/org/freedesktop/UPower"
            );

            foreach (var path in proxy.enumerate_devices())
                _devices.set(path, new Device(path));

            proxy.device_added.connect((path) => {
                try {
                    var d = new Device(path);
                    _devices.set(path, d);
                    device_added(d);
                    notify_property("devices");
                } catch (Error err) {
                    critical(err.message);
                }
            });

            proxy.device_removed.connect((path) => {
                device_removed(_devices.get(path));
                _devices.remove(path);
                notify_property("devices");
            });
        } catch (Error error) {
            critical(error.message);
        }
    }
}
