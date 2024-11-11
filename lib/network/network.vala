namespace AstalNetwork {
    public Network get_default() {
        return Network.get_default();
    }
}

public class AstalNetwork.Network : Object {
    private static Network instance;
    public static Network get_default() {
        if (instance == null)
            instance = new Network();

        return instance;
    }

    public NM.Client client { get; private set; }

    public Wifi? wifi { get; private set; }
    public Wired? wired { get; private set; }
    public Primary primary { get; private set; }

    public Connectivity connectivity {
        get { return (Connectivity)client.connectivity; }
    }

    public State state {
        get { return (State)client.state; }
    }

    construct {
        try {
            client = new NM.Client();
            var wifi_device = (NM.DeviceWifi)get_device(NM.DeviceType.WIFI);
            if (wifi_device != null)
                wifi = new Wifi(wifi_device);

            var ethernet = (NM.DeviceEthernet)get_device(NM.DeviceType.ETHERNET);
            if (ethernet != null)
                wired = new Wired(ethernet);

            sync();
            client.notify["primary-connection"].connect(sync);
            client.notify["activating-connection"].connect(sync);

            client.notify["state"].connect(() => notify_property("state"));
            client.notify["connectivity"].connect(() => notify_property("connectivity"));
        } catch (Error err) {
            critical(err.message);
        }
    }

    private NM.Device? get_device(NM.DeviceType t) {
        var valid = new GenericArray<NM.Device>();
        foreach (var device in client.get_devices()) {
            if (device.device_type == t)
                valid.add(device);
        }

        foreach (var device in valid) {
            if (device.active_connection != null)
                return device;
        }

        if (valid.length > 0)
            return valid.get(0);

        return null;
    }

    private void sync() {
        var ac = client.get_primary_connection();

        if (ac == null)
            ac = client.get_activating_connection();

        if (ac != null)
            primary = Primary.from_connection_type(ac.type);
        else
            primary = Primary.UNKNOWN;
    }
}

public enum AstalNetwork.Primary {
    UNKNOWN,
    WIRED,
    WIFI;

    public string to_string() {
        switch (this) {
            case WIFI: return "wifi";
            case WIRED: return "wired";
            default: return "unknown";
        }
    }

    public static Primary from_connection_type(string type) {
        switch (type) {
            case "802-11-wireless": return Primary.WIFI;
            case "802-3-ethernet": return Primary.WIRED;
            default: return Primary.UNKNOWN;
        }
    }
}

// alias for NM.State
public enum AstalNetwork.State {
    UNKNOWN,
    ASLEEP,
    DISCONNECTED,
    DISCONNECTING,
    CONNECTING,
    CONNECTED_LOCAL,
    CONNECTED_SITE,
    CONNECTED_GLOBAL;

    public string to_string() {
        switch (this) {
            case ASLEEP: return "asleep";
            case DISCONNECTED: return "disconnected";
            case DISCONNECTING: return "disconnecting";
            case CONNECTING: return "connecting";
            case CONNECTED_LOCAL: return "connected_local";
            case CONNECTED_SITE: return "connected_site";
            case CONNECTED_GLOBAL: return "connected_global";
            default: return "unknown";
        }
    }
}


// alias for NM.ConnectivityState
public enum AstalNetwork.Connectivity {
    UNKNOWN,
    NONE,
    PORTAL,
    LIMITED,
    FULL;

    public string to_string() {
        switch (this) {
            case NONE: return "none";
            case PORTAL: return "portal";
            case LIMITED: return "limited";
            case FULL: return "full";
            default: return "unknown";
        }
    }
}

// alias for NM.DeviceState
public enum AstalNetwork.DeviceState {
    UNKNOWN = 0,
    UNMANAGED = 10,
    UNAVAILABLE = 20,
    DISCONNECTED = 30,
    PREPARE = 40,
    CONFIG = 50,
    NEED_AUTH = 60,
    IP_CONFIG = 70,
    IP_CHECK = 80,
    SECONDARIES = 90,
    ACTIVATED = 100,
    DEACTIVATING = 110,
    FAILED = 120;

    public string to_string() {
        switch (this) {
            case UNMANAGED: return "unmanaged";
            case UNAVAILABLE: return "unavailable";
            case DISCONNECTED: return "disconnected";
            case PREPARE: return "prepare";
            case CONFIG: return "config";
            case NEED_AUTH: return "need_auth";
            case IP_CONFIG: return "ip_config";
            case IP_CHECK: return "ip_check";
            case SECONDARIES: return "secondaries";
            case ACTIVATED: return "activated";
            case DEACTIVATING: return "deactivating";
            case FAILED: return "failed";
            default: return "unknown";
        }

    }
}

public enum AstalNetwork.Internet {
    CONNECTED,
    CONNECTING,
    DISCONNECTED;

    public static Internet from_device(NM.Device device) {
        if (device == null || device.active_connection == null)
            return DISCONNECTED;

        switch (device.active_connection.state) {
            case NM.ActiveConnectionState.ACTIVATED: return CONNECTED;
            case NM.ActiveConnectionState.ACTIVATING: return CONNECTING;
            default: return DISCONNECTED;
        }
    }

    public string to_string() {
        switch (this) {
            case CONNECTED: return "connected";
            case CONNECTING: return "connecting";
            default: return "disconnected";
        }
    }
}
