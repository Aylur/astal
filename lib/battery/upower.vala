namespace AstalBattery {
public class UPower : Object {
    private IUPower proxy;
    private HashTable<string, Device> _devices =
        new HashTable<string, Device>(str_hash, str_equal);

    public List<weak Device> devices {
        owned get { return _devices.get_values(); }
    }

    public signal void device_added(Device device);
    public signal void device_removed(Device device);

    public Device display_device { owned get { return Device.get_default(); }}

    public string daemon_version { owned get { return proxy.daemon_version; } }
    public bool on_battery { get { return proxy.on_battery; } }
    public bool lid_is_closed { get { return proxy.lid_is_closed; } }
    public bool lis_is_present { get { return proxy.lid_is_closed; } }

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
                _devices.set(path, new Device(path));
                notify_property("devices");
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
}
