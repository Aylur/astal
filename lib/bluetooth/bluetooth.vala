namespace AstalBluetooth {
    /**
     * Gets the default singleton Bluetooth object.
     */
    public Bluetooth get_default() {
        return Bluetooth.get_default();
    }
}

/**
 * Manager object for `org.bluez`.
 */
public class AstalBluetooth.Bluetooth : Object {
    private static Bluetooth _instance;

    /**
     * Gets the default singleton Bluetooth object.
     */
    public static Bluetooth get_default() {
        if (_instance == null)
            _instance = new Bluetooth();

        return _instance;
    }

    private DBusObjectManagerClient manager;

    private HashTable<string, Adapter> _adapters =
        new HashTable<string, Adapter>(str_hash, str_equal);

    private HashTable<string, Device> _devices =
        new HashTable<string, Device>(str_hash, str_equal);

    /**
     * Emitted when a new device is registered on the `org.bluez` bus.
     */
    public signal void device_added (Device device) {
        notify_property("devices");
    }

    /**
     * Emitted when a device is unregistered on the `org.bluez` bus.
     */
    public signal void device_removed (Device device) {
        notify_property("devices");
    }

    /**
     * Emitted when an adapter is registered on the `org.bluez` bus.
     */
    public signal void adapter_added (Adapter adapter) {
        notify_property("adapters");
    }

    /**
     * Emitted when an adapter is unregistered on the `org.bluez` bus.
     */
    public signal void adapter_removed (Adapter adapter) {
        notify_property("adapters");
    }

    /**
     * `true` if any of the [property@AstalBluetooth.Bluetooth:adapters] are powered.
     */
    public bool is_powered { get; private set; default = false; }

    /**
     * `true` if any of the [property@AstalBluetooth.Bluetooth:devices] is connected.
     */
    public bool is_connected { get; private set; default = false; }

    /**
     * The first registered adapter which is usually the only adapter.
     */
    public Adapter? adapter { get { return adapters.nth_data(0); } }

    /**
     * List of adapters available on the host device.
     */
    public List<weak Adapter> adapters {
        owned get { return _adapters.get_values(); }
    }

    /**
     * List of registered devices on the `org.bluez` bus.
     */
    public List<weak Device> devices {
        owned get { return _devices.get_values(); }
    }

    construct {
        try {
            manager = new DBusObjectManagerClient.for_bus_sync(
                BusType.SYSTEM,
                DBusObjectManagerClientFlags.NONE,
                "org.bluez",
                "/",
                manager_proxy_get_type,
                null
            );

            foreach (var object in manager.get_objects()) {
                foreach (var iface in object.get_interfaces()) {
                    on_interface_added(object, iface);
                }
            }

            manager.interface_added.connect(on_interface_added);
            manager.interface_removed.connect(on_interface_removed);

            manager.object_added.connect((object) => {
                foreach (var iface in object.get_interfaces()) {
                    on_interface_added(object, iface);
                }
            });

            manager.object_removed.connect((object) => {
                foreach (var iface in object.get_interfaces()) {
                    on_interface_removed(object, iface);
                }
            });
        } catch (Error err) {
            critical(err.message);
        }
    }

    /**
     * Toggle the [property@AstalBluetooth.Adapter:powered]
     * property of the [property@AstalBluetooth.Bluetooth:adapter].
     */
    public void toggle() {
        adapter.powered = !adapter.powered;
    }

    [CCode (cname="astal_bluetooth_idevice_proxy_get_type")]
    extern static GLib.Type get_idevice_proxy_type();

    [CCode (cname="astal_bluetooth_iadapter_proxy_get_type")]
    extern static GLib.Type get_iadapter_proxy_type();

    private Type manager_proxy_get_type(DBusObjectManagerClient _, string object_path, string? interface_name) {
        if (interface_name == null)
            return typeof(DBusObjectProxy);

        switch (interface_name) {
            case "org.bluez.Device1":
                return get_idevice_proxy_type();
            case "org.bluez.Adapter1":
                return get_iadapter_proxy_type();
            default:
                return typeof(DBusProxy);
        }
    }

    private void on_interface_added(DBusObject object, DBusInterface iface) {
        if (iface is IDevice) {
            var device = new Device((IDevice)iface);
            _devices.set(device.object_path, device);
            device_added(device);
            device.notify.connect(sync);
            sync();
        }

        if (iface is IAdapter) {
            var adapter = new Adapter((IAdapter)iface);
            _adapters.set(adapter.object_path, adapter);
            adapter_added(adapter);
            adapter.notify.connect(sync);
            sync();
        }
    }

    private void on_interface_removed (DBusObject object, DBusInterface iface) {
        if (iface is IDevice) {
            unowned var device = (IDevice)iface;
            device_removed(_devices.get(device.g_object_path));
            _devices.remove(device.g_object_path);
        }

        if (iface is IAdapter) {
            unowned var adapter = (IAdapter)iface;
            adapter_removed(_adapters.get(adapter.g_object_path));
            _adapters.remove(adapter.g_object_path);
        }

        sync();
    }

    private void sync() {
        var powered = get_powered();
        var connected = get_connected();

        if (powered != is_powered || connected != is_connected) {
            if (powered != is_powered) {
                is_powered = powered;
            }

            if (connected != is_connected) {
                is_connected = connected;
            }
        }
    }

    private bool get_powered() {
        foreach (var adapter in adapters) {
            if (adapter.powered) {
                return true;
            }
        }

        return false;
    }

    private bool get_connected() {
        foreach (var device in devices) {
            if (device.connected) {
                return true;
            }
        }

        return false;
    }
}
