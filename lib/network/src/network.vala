namespace AstalNetwork {
    /**
     * Get the singleton [class@AstalNetwork.Network] instance.
     */
    public Network get_default() {
        return Network.get_default();
    }
}

/**
 * A simple abstraction over NetworkManager.
 * Exposes the main wifi and wired devices and the overall network state.
 */
public class AstalNetwork.Network : Object {
    private static Network instance;

    /**
     * Get the singleton instance.
     */
    public static Network get_default() {
        if (instance == null) instance = new Network();
        return instance;
    }

    /**
     * The underlying NetworkManager client.
     */
    public NM.Client client { get; private set; }

    /**
     * Proxy for the main wireless device.
     * It is always present, its [property@AstalNetwork.Wifi:device] is `null` when there is no wifi device.
     */
    public Wifi wifi { get; private set; }

    /**
     * Proxy for the main ethernet device.
     * It is always present, its [property@AstalNetwork.Wired:device] is `null` when there is no ethernet device.
     */
    public Wired wired { get; private set; }

    /**
     * The type of the connection NetworkManager uses for the default route.
     */
    public Primary primary { get; private set; }

    /**
     * The connectivity state.
     */
    public Connectivity connectivity {
        get { return (Connectivity)client.connectivity; }
    }

    /**
     * The overall networking state.
     */
    public State state {
        get { return (State)client.state; }
    }

    private Network() {}
    construct {
        try {
            client = new NM.Client();
            wifi = new Wifi(client);
            wired = new Wired(client);

            sync_devices();
            client.device_added.connect(() => sync_devices());
            client.device_removed.connect((device) => sync_devices((NM.Device)device));

            sync();
            client.notify["primary-connection"].connect(sync);
            client.notify["activating-connection"].connect(sync);

            client.notify["state"].connect(() => notify_property("state"));
            client.notify["connectivity"].connect(() => notify_property("connectivity"));
        } catch (Error err) {
            critical(err.message);
        }
    }

    private void sync_devices(NM.Device? removed = null) {
        wifi.set_device((NM.DeviceWifi?)get_device(NM.DeviceType.WIFI, removed));
        wired.set_device((NM.DeviceEthernet?)get_device(NM.DeviceType.ETHERNET, removed));
    }

    private NM.Device? get_device(NM.DeviceType t, NM.Device? exclude = null) {
        var valid = new GenericArray<NM.Device>();
        foreach (var device in client.get_devices()) {
            if ((device.device_type == t) && (device != exclude)) {
                valid.add(device);
            }
        }

        foreach (var device in valid) {
            if (device.active_connection != null) {
                return device;
            }
        }

        if (valid.length > 0) return valid.get(0);

        return null;
    }

    private void sync() {
        var ac = client.get_primary_connection();

        if (ac == null) ac = client.get_activating_connection();

        if (ac != null) {
            primary = Primary.from_connection_type(ac.type);
        } else {
            primary = Primary.UNKNOWN;
        }
    }
}

/**
 * The type of the primary connection.
 */
public enum AstalNetwork.Primary {
    /** No primary connection, or it is not wired or wifi. */
    UNKNOWN,
    /** Wired ethernet connection. */
    WIRED,
    /** Wireless connection. */
    WIFI;

    public string to_string() {
        switch (this) {
            case WIFI:
                return "wifi";
            case WIRED:
                return "wired";
            default:
                return "unknown";
        }
    }

    /**
     * Get the type from a NetworkManager connection type, such as `802-11-wireless`.
     */
    public static Primary from_connection_type(string type) {
        switch (type) {
            case "802-11-wireless":
                return Primary.WIFI;
            case "802-3-ethernet":
                return Primary.WIRED;
            default:
                return Primary.UNKNOWN;
        }
    }
}

/**
 * Overall networking state, an alias for `NM.State`.
 */
public enum AstalNetwork.State {
    /** The state is unknown. */
    UNKNOWN = 0,
    /** Networking is disabled. */
    ASLEEP = 10,
    /** There is no active network connection. */
    DISCONNECTED = 20,
    /** Network connections are being cleaned up. */
    DISCONNECTING = 30,
    /** A network connection is being started. */
    CONNECTING = 40,
    /** There is only local IPv4 and/or IPv6 connectivity. */
    CONNECTED_LOCAL = 50,
    /** There is only site-wide IPv4 and/or IPv6 connectivity. */
    CONNECTED_SITE = 60,
    /** There is global IPv4 and/or IPv6 internet connectivity. */
    CONNECTED_GLOBAL = 70;

    public string to_string() {
        switch (this) {
            case ASLEEP:
                return "asleep";
            case DISCONNECTED:
                return "disconnected";
            case DISCONNECTING:
                return "disconnecting";
            case CONNECTING:
                return "connecting";
            case CONNECTED_LOCAL:
                return "connected_local";
            case CONNECTED_SITE:
                return "connected_site";
            case CONNECTED_GLOBAL:
                return "connected_global";
            default:
                return "unknown";
        }
    }
}

/**
 * Connectivity state as checked by NetworkManager, an alias for `NM.ConnectivityState`.
 */
public enum AstalNetwork.Connectivity {
    /** Connectivity checks are disabled or have not run yet. */
    UNKNOWN,
    /** The host is not connected to any network. */
    NONE,
    /** The internet connection is hijacked by a captive portal. */
    PORTAL,
    /** The host is connected to a network, but it has no access to the internet. */
    LIMITED,
    /** The host is connected to a network and has full access to the internet. */
    FULL;

    public string to_string() {
        switch (this) {
            case NONE:
                return "none";
            case PORTAL:
                return "portal";
            case LIMITED:
                return "limited";
            case FULL:
                return "full";
            default:
                return "unknown";
        }
    }
}

/**
 * State of a network device, an alias for `NM.DeviceState`.
 */
public enum AstalNetwork.DeviceState {
    /** The device state is unknown. */
    UNKNOWN = 0,
    /** The device is not managed by NetworkManager. */
    UNMANAGED = 10,
    /** The device cannot be used, for example the wifi radio is disabled. */
    UNAVAILABLE = 20,
    /** The device can be activated, but it is not connected. */
    DISCONNECTED = 30,
    /** The device is preparing the connection. */
    PREPARE = 40,
    /** The device is connecting to the network. */
    CONFIG = 50,
    /** The device needs more information to connect, such as a password. */
    NEED_AUTH = 60,
    /** The device is requesting IPv4 and/or IPv6 addresses. */
    IP_CONFIG = 70,
    /** The device is checking whether further action is required. */
    IP_CHECK = 80,
    /** The device is waiting for a secondary connection, such as a VPN. */
    SECONDARIES = 90,
    /** The device is connected. */
    ACTIVATED = 100,
    /** The device is disconnecting. */
    DEACTIVATING = 110,
    /** The device failed to connect. */
    FAILED = 120;

    public string to_string() {
        switch (this) {
            case UNMANAGED:
                return "unmanaged";
            case UNAVAILABLE:
                return "unavailable";
            case DISCONNECTED:
                return "disconnected";
            case PREPARE:
                return "prepare";
            case CONFIG:
                return "config";
            case NEED_AUTH:
                return "need_auth";
            case IP_CONFIG:
                return "ip_config";
            case IP_CHECK:
                return "ip_check";
            case SECONDARIES:
                return "secondaries";
            case ACTIVATED:
                return "activated";
            case DEACTIVATING:
                return "deactivating";
            case FAILED:
                return "failed";
            default:
                return "unknown";
        }
    }
}

/**
 * Connection state of a device's active connection.
 */
public enum AstalNetwork.Internet {
    /** The connection is activated. */
    CONNECTED,
    /** The connection is being activated. */
    CONNECTING,
    /** There is no active connection. */
    DISCONNECTED;

    /**
     * Get the state of the active connection of a device.
     */
    public static Internet from_device(NM.Device? device) {
        if ((device == null) || (device.active_connection == null)) {
            return DISCONNECTED;
        }

        switch (device.active_connection.state) {
            case NM.ActiveConnectionState.ACTIVATED:
                return CONNECTED;
            case NM.ActiveConnectionState.ACTIVATING:
                return CONNECTING;
            default:
                return DISCONNECTED;
        }
    }

    public string to_string() {
        switch (this) {
            case CONNECTED:
                return "connected";
            case CONNECTING:
                return "connecting";
            default:
                return "disconnected";
        }
    }
}
