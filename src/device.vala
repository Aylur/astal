namespace AstalBluetooth {
[DBus (name = "org.bluez.Device1")]
internal interface IDevice : DBusProxy {
    public abstract void cancel_pairing() throws Error;
    public abstract async void connect() throws Error;
    public abstract void connect_profile(string uuid) throws Error;
    public abstract async void disconnect() throws Error;
    public abstract void disconnect_profile(string uuid) throws Error;
    public abstract void pair() throws Error;

    public abstract string[] uuids { owned get; }
    public abstract bool blocked { get; set; }
    public abstract bool connected { get; }
    public abstract bool legacy_pairing { get; }
    public abstract bool paired { get; }
    public abstract bool trusted { get; set; }
    public abstract int16 rssi { get; }
    public abstract ObjectPath adapter { owned get; }
    public abstract string address { owned get; }
    public abstract string alias { owned get; set; }
    public abstract string icon { owned get; }
    public abstract string modalias { owned get; }
    public abstract string name { owned get; }
    public abstract uint16 appearance { get; }
    public abstract uint32 class { get; }
}

public class Device : Object {
    private IDevice proxy;
    public string object_path { owned get; construct set; }

    internal Device(IDevice proxy) {
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
    public bool connected { get { return proxy.connected; } }
    public bool legacy_pairing { get { return proxy.legacy_pairing; } }
    public bool paired { get { return proxy.paired; } }
    public int16 rssi { get { return proxy.rssi; } }
    public ObjectPath adapter { owned get { return proxy.adapter; } }
    public string address { owned get { return proxy.address; } }
    public string icon { owned get { return proxy.icon; } }
    public string modalias { owned get { return proxy.modalias; } }
    public string name { owned get { return proxy.name; } }
    public uint16 appearance { get { return proxy.appearance; } }
    public uint32 class { get { return proxy.class; } }
    public bool connecting { get; private set; }

    public bool blocked {
        get { return proxy.blocked; }
        set { proxy.blocked = value; }
    }

    public bool trusted {
        get { return proxy.trusted; }
        set { proxy.trusted = value; }
    }

    public string alias {
        owned get { return proxy.alias; }
        set { proxy.alias = value; }
    }

    public void cancel_pairing() {
        try { proxy.cancel_pairing(); } catch (Error err) { critical(err.message); }
    }

    public async void connect_device() {
        try {
            connecting = true;
            yield proxy.connect();
        } catch (Error err) {
            critical(err.message);
        } finally {
            connecting = false;
        }
    }

    public async void disconnect_device() {
        try { yield proxy.disconnect(); } catch (Error err) { critical(err.message); }
    }

    public void connect_profile(string uuid) {
        try { proxy.connect_profile(uuid); } catch (Error err) { critical(err.message); }
    }

    public void disconnect_profile(string uuid) {
        try { proxy.disconnect_profile(uuid); } catch (Error err) { critical(err.message); }
    }

    public void pair() {
        try { proxy.pair(); } catch (Error err) { critical(err.message); }
    }
}
}
