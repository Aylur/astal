namespace AstalBluetooth {
[DBus (name = "org.bluez.Adapter1")]
internal interface IAdapter : DBusProxy {
    public abstract void remove_device(ObjectPath device) throws Error;
    public abstract void start_discovery() throws Error;
    public abstract void stop_discovery() throws Error;

    public abstract string[] uuids { owned get; }
    public abstract bool discoverable { get; set; }
    public abstract bool discovering { get; }
    public abstract bool pairable { get; set; }
    public abstract bool powered { get; set; }
    public abstract string address { owned get; }
    public abstract string alias { owned get; set; }
    public abstract string modalias { owned get; }
    public abstract string name { owned get; }
    public abstract uint class { get; }
    public abstract uint discoverable_timeout { get; set; }
    public abstract uint pairable_timeout { get; set; }
}

public class Adapter : Object {
    private IAdapter proxy;
    public string object_path { owned get; construct set; }

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

    public string[] uuids { owned get { return proxy.uuids; } }
    public bool discovering { get { return proxy.discovering; } }
    public string modalias { owned get { return proxy.modalias; } }
    public string name { owned get { return proxy.name; } }
    public uint class { get { return proxy.class; } }
    public string address { owned get { return proxy.address; } }

    public bool discoverable {
        get { return proxy.discoverable; }
        set { proxy.discoverable = value; }
    }

    public bool pairable {
        get { return proxy.pairable; }
        set { proxy.pairable = value; }
    }

    public bool powered {
        get { return proxy.powered; }
        set { proxy.powered = value; }
    }

    public string alias {
        owned get { return proxy.alias; }
        set { proxy.alias = value; }
    }

    public uint discoverable_timeout {
        get { return proxy.discoverable_timeout; }
        set { proxy.discoverable_timeout = value; }
    }

    public uint pairable_timeout {
        get { return proxy.pairable_timeout; }
        set { proxy.pairable_timeout = value; }
    }

    public void remove_device(Device device) {
        try { proxy.remove_device((ObjectPath)device.object_path); } catch (Error err) { critical(err.message); }
    }

    public void start_discovery() {
        try { proxy.start_discovery(); } catch (Error err) { critical(err.message); }
    }

    public void stop_discovery() {
        try { proxy.stop_discovery(); } catch (Error err) { critical(err.message); }
    }
}
}
