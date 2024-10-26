/**
 * Object representing an [[https://github.com/RadiusNetworks/bluez/blob/master/doc/adapter-api.txt|adapter]].
 */
public class AstalBluetooth.Adapter : Object {
    private IAdapter proxy;

    internal string object_path { owned get; private set; }

    internal Adapter(IAdapter proxy) {
        this.proxy = proxy;
        this.object_path = proxy.g_object_path;
        proxy.g_properties_changed.connect((props) => {
            var map = (HashTable<string, Variant>)props;
            foreach (var key in map.get_keys()) {
                var prop = kebab_case(key);
                if (get_class().find_property(prop) != null) {
                    notify_property(prop);
                }
            }
        });
    }

    /**
     * List of 128-bit UUIDs that represents the available local services.
     */
    public string[] uuids { owned get { return proxy.uuids; } }

    /**
     * Indicates that a device discovery procedure is active.
     */
    public bool discovering { get { return proxy.discovering; } }

    /**
     * Local Device ID information in modalias format used by the kernel and udev.
     */
    public string modalias { owned get { return proxy.modalias; } }

    /**
     * The Bluetooth system name (pretty hostname).
     */
    public string name { owned get { return proxy.name; } }

    /**
     * The Bluetooth class of device.
     */
    public uint class { get { return proxy.class; } }

    /**
     * The Bluetooth device address.
     */
    public string address { owned get { return proxy.address; } }


    /**
     * Switch an adapter to discoverable or non-discoverable
     * to either make it visible or hide it.
     */
    public bool discoverable {
        get { return proxy.discoverable; }
        set { proxy.discoverable = value; }
    }


    /**
     * Switch an adapter to pairable or non-pairable.
     */
    public bool pairable {
        get { return proxy.pairable; }
        set { proxy.pairable = value; }
    }


    /**
     * Switch an adapter on or off.
     */
    public bool powered {
        get { return proxy.powered; }
        set { proxy.powered = value; }
    }


    /**
     * The Bluetooth friendly name.
     *
     * In case no alias is set, it will return [property@AstalBluetooth.Adapter:name].
     */
    public string alias {
        owned get { return proxy.alias; }
        set { proxy.alias = value; }
    }


    /**
     * The discoverable timeout in seconds.
     * A value of zero means that the timeout is disabled
     * and it will stay in discoverable/limited mode forever
     * until [method@AstalBluetooth.Adapter.stop_discovery] is invoked.
     * The default value for the discoverable timeout should be `180`.
     */
    public uint discoverable_timeout {
        get { return proxy.discoverable_timeout; }
        set { proxy.discoverable_timeout = value; }
    }


    /**
     * The pairable timeout in seconds.
     *
     * A value of zero means that the timeout is disabled and it will stay in pairable mode forever.
     * The default value for pairable timeout should be disabled `0`.
     */
    public uint pairable_timeout {
        get { return proxy.pairable_timeout; }
        set { proxy.pairable_timeout = value; }
    }


    /**
     * This removes the remote device and the pairing information.
     *
     * Possible errors: `InvalidArguments`, `Failed`.
     */
    public void remove_device(Device device) throws Error {
        proxy.remove_device(device.object_path);
    }


    /**
     * This method starts the device discovery procedure.
     *
     * Possible errors: `NotReady`, `Failed`.
     */
    public void start_discovery() throws Error {
        proxy.start_discovery();
    }


    /**
     * This method will cancel any previous [method@AstalBluetooth.Adapter.start_discovery] procedure.
     *
     * Possible errors: `NotReady`, `Failed`, `NotAuthorized`.
     */
    public void stop_discovery() throws Error {
        proxy.stop_discovery();
    }
}
