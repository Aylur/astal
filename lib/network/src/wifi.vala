public class AstalNetwork.Wifi : Object {
    internal const string ICON_EXCELLENT = "network-wireless-signal-excellent-symbolic";
    internal const string ICON_OK = "network-wireless-signal-ok-symbolic";
    internal const string ICON_GOOD = "network-wireless-signal-good-symbolic";
    internal const string ICON_WEAK = "network-wireless-signal-weak-symbolic";
    internal const string ICON_NONE = "network-wireless-signal-none-symbolic";
    internal const string ICON_ACQUIRING = "network-wireless-acquiring-symbolic";
    internal const string ICON_CONNECTED = "network-wireless-connected-symbolic";
    internal const string ICON_DISABLED = "network-wireless-disabled-symbolic";
    internal const string ICON_OFFLINE = "network-wireless-offline-symbolic";
    internal const string ICON_NO_ROUTE = "network-wireless-no-route-symbolic";
    internal const string ICON_HOTSPOT = "network-wireless-hotspot-symbolic";

    private HashTable<string, AccessPoint> _access_points =
        new HashTable<string, AccessPoint>(str_hash, str_equal);

    public NM.DeviceWifi device { get; construct set; }

    public NM.ActiveConnection? active_connection { get; private set; }
    private ulong connection_handler = 0;
    private ulong device_active_connection_handler = 0;

    public AccessPoint? active_access_point { get; private set; }
    private ulong ap_handler = 0;
    private ulong device_active_access_point_handler = 0;
    private ulong device_access_point_added_handler = 0;
    private ulong device_access_point_removed_handler = 0;
    private ulong device_state_handler = 0;
    private ulong client_wireless_handler = 0;
    private ulong client_connectivity_handler = 0;

    public List<weak AccessPoint> access_points {
        owned get { return _access_points.get_values(); }
    }

    public bool enabled {
        get { return device.client.wireless_enabled; }
        set { device.client.wireless_enabled = value; }
    }

    public Internet internet { get; private set; }
    public uint bandwidth { get; private set; }
    public string ssid { get; private set; }
    public uint8 strength { get; private set; }
    public uint frequency { get; private set; }
    public DeviceState state { get; private set; }
    public string icon_name { get; private set; }
    public bool is_hotspot { get; private set; }
    public bool scanning { get; private set; }

    public signal void access_point_added(AccessPoint ap) ;
    public signal void access_point_removed(AccessPoint ap) ;

    internal Wifi(NM.DeviceWifi device) {
        this.device = device;

        foreach (var ap in device.access_points) {
            var new_ap = new AccessPoint(this, ap);
            _access_points.set(ap.bssid, new_ap);
            access_point_added(new_ap);
        }

        device_access_point_added_handler = device.access_point_added.connect((access_point) => {
            var ap = (NM.AccessPoint)access_point;
            var new_ap = new AccessPoint(this, ap);
            _access_points.set(ap.bssid, new_ap);
            access_point_added(new_ap);
            notify_property("access-points");
        });

        device_access_point_removed_handler = device.access_point_removed.connect((access_point) => {
            var ap = (NM.AccessPoint)access_point;
            var rem_ap = _access_points.get(ap.bssid);
            _access_points.remove(ap.bssid);
            if (rem_ap != null) rem_ap.disconnect_signals();
            if (rem_ap != null) access_point_removed(rem_ap);
            notify_property("access-points");
        });

        on_active_connection();
        device_active_connection_handler = device.notify["active-connection"].connect(on_active_connection);

        on_active_access_point();
        device_active_access_point_handler =
            device.notify["active-access-point"].connect(on_active_access_point);

        state = (DeviceState)device.state;
        client_wireless_handler = device.client.notify["wireless-enabled"].connect(() => {
            notify_property("enabled");
            icon_name = _icon();
        });
        device_state_handler = device.state_changed.connect((n, o, r) => {
            state_changed(n, o, r);
            state = (DeviceState)n;
        });

        client_connectivity_handler =
            device.client.notify["connectivity"].connect(() => { icon_name = _icon(); });
        icon_name = _icon();
    }

    public signal void state_changed(
        DeviceState new_state,
        DeviceState old_state,
        NM.DeviceStateReason reaseon
    );

    internal void disconnect_signals() {
        if ((connection_handler > 0) && (active_connection != null)) {
            SignalHandler.disconnect(active_connection, connection_handler);
            connection_handler = 0;
        }
        active_connection = null;

        if ((ap_handler > 0) && (active_access_point != null)) {
            SignalHandler.disconnect(active_access_point, ap_handler);
            ap_handler = 0;
        }
        active_access_point = null;

        if (device_active_connection_handler > 0) {
            SignalHandler.disconnect(device, device_active_connection_handler);
            device_active_connection_handler = 0;
        }

        if (device_active_access_point_handler > 0) {
            SignalHandler.disconnect(device, device_active_access_point_handler);
            device_active_access_point_handler = 0;
        }

        if (device_access_point_added_handler > 0) {
            SignalHandler.disconnect(device, device_access_point_added_handler);
            device_access_point_added_handler = 0;
        }

        if (device_access_point_removed_handler > 0) {
            SignalHandler.disconnect(device, device_access_point_removed_handler);
            device_access_point_removed_handler = 0;
        }

        if (device_state_handler > 0) {
            SignalHandler.disconnect(device, device_state_handler);
            device_state_handler = 0;
        }

        if (client_wireless_handler > 0) {
            SignalHandler.disconnect(device.client, client_wireless_handler);
            client_wireless_handler = 0;
        }

        if (client_connectivity_handler > 0) {
            SignalHandler.disconnect(device.client, client_connectivity_handler);
            client_connectivity_handler = 0;
        }

        foreach (var ap in _access_points.get_values()) {
            ap.disconnect_signals();
        }
        _access_points.remove_all();
    }

    public void scan() {
        scanning = true;
        var last_scan = device.last_scan;
        device.request_scan_async.begin(null, (_, res) => {
            try {
                device.request_scan_async.end(res);
                Timeout.add(1000, () => {
                    if (device.last_scan == last_scan) return Source.CONTINUE;

                    scanning = false;
                    return Source.REMOVE;
                }, Priority.DEFAULT);
            } catch (Error err) {
                critical(err.message);
            }
        });
    }

    public async void deactivate_connection() throws Error {
        if (device.active_connection == null) {
            return;
        }

        yield device.client.deactivate_connection_async(device.active_connection, null);
    }

    private void on_active_connection() {
        if ((connection_handler > 0) && (active_connection != null)) {
            SignalHandler.disconnect(active_connection, connection_handler);
            connection_handler = 0;
            active_connection = null;
        }

        active_connection = device.active_connection;
        is_hotspot = _hotspot();
        internet = Internet.from_device(device);
        if (active_connection != null) {
            connection_handler = active_connection.notify["state"].connect(() => {
                internet = Internet.from_device(device);
                icon_name = _icon();
            });
        }
        icon_name = _icon();
    }

    private void on_active_access_point_notify() {
        bandwidth = active_access_point.bandwidth;
        frequency = active_access_point.frequency;
        strength = active_access_point.strength;
        ssid = active_access_point.ssid;
        icon_name = _icon();
    }

    private void on_active_access_point() {
        if ((ap_handler > 0) && (active_access_point != null)) {
            SignalHandler.disconnect(active_access_point, ap_handler);
            ap_handler = 0;
            active_access_point = null;
        }

        var ap = device.active_access_point;
        if (ap != null) {
            active_access_point = _access_points.get(ap.bssid);
            if (active_access_point != null) {
                on_active_access_point_notify();
                ap_handler = active_access_point.notify.connect(on_active_access_point_notify);
            }
        }
    }

    private string _icon() {
        if (!enabled) return ICON_DISABLED;

        var full = device.client.connectivity == NM.ConnectivityState.FULL;

        if (internet == Internet.CONNECTED) {
            if (is_hotspot) return ICON_HOTSPOT;
            if (!full) return ICON_NO_ROUTE;
            if (active_access_point == null) return ICON_CONNECTED;

            if (strength >= 80) return ICON_EXCELLENT;
            if (strength >= 60) return ICON_GOOD;
            if (strength >= 40) return ICON_OK;
            if (strength >= 20) return ICON_WEAK;

            return ICON_NONE;
        }

        if (internet == Internet.CONNECTING) {
            return ICON_ACQUIRING;
        }

        return ICON_OFFLINE;
    }

    private bool _hotspot() {
        if (device.active_connection == null) return false;

        var conn = device.active_connection.connection;
        if (conn == null) return false;

        var ip4config = conn.get_setting_ip4_config();
        if (ip4config == null) return false;

        return ip4config.method == NM.SettingIP4Config.METHOD_SHARED;
    }
}
