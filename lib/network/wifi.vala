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

    public AccessPoint? active_access_point { get; private set; }
    private ulong ap_handler = 0;

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

    internal Wifi(NM.DeviceWifi device) {
        this.device = device;

        foreach (var ap in device.access_points)
            _access_points.set(ap.bssid, new AccessPoint(this, ap));

        device.access_point_added.connect((access_point) => {
            var ap = (NM.AccessPoint)access_point;
            _access_points.set(ap.bssid, new AccessPoint(this, ap));
            notify_property("access-points");
        });

        device.access_point_removed.connect((access_point) => {
            var ap = (NM.AccessPoint)access_point;
            _access_points.remove(ap.bssid);
            notify_property("access-points");
        });

        on_active_connection();
        device.notify["active-connection"].connect(on_active_connection);

        on_active_access_point();
        device.notify["active-access-point"].connect(on_active_access_point);

        state = (DeviceState)device.state;
        device.client.notify["wireless-enabled"].connect(() => notify_property("enabled"));
        device.state_changed.connect((n, o, r) => {
            state_changed(n, o, r);
            state = (DeviceState)n;
        });

        device.notify.connect(() => { icon_name = _icon(); });
        device.client.notify.connect(() => { icon_name = _icon(); });
        icon_name = _icon();
    }

    public signal void state_changed(
        DeviceState new_state,
        DeviceState old_state,
        NM.DeviceStateReason reaseon
    );

    public void scan() {
        scanning = true;
        var last_scan = device.last_scan;
        device.request_scan_async.begin(null, (_, res) => {
            try {
                device.request_scan_async.end(res);
                Timeout.add(1000, () => {
                    if (device.last_scan == last_scan)
                        return Source.CONTINUE;

                    scanning = false;
                    return Source.REMOVE;
                }, Priority.DEFAULT);
            } catch (Error err) {
                critical(err.message);
            }
        });
    }

    private void on_active_connection() {
        if (connection_handler > 0 && active_connection != null) {
            active_connection.disconnect(connection_handler);
            connection_handler = 0;
            active_connection = null;
        }

        active_connection = device.active_connection;
        is_hotspot = _hotspot();
        if (active_connection != null) {
            connection_handler = active_connection.notify["state"].connect(() => {
                internet = Internet.from_device(device);
            });
        }
    }

    private void on_active_access_point_notify() {
        bandwidth = active_access_point.bandwidth;
        frequency = active_access_point.frequency;
        strength = active_access_point.strength;
        ssid = active_access_point.ssid;
    }

    private void on_active_access_point() {
        if (ap_handler > 0 && active_access_point != null) {
            active_access_point.disconnect(ap_handler);
            ap_handler = 0;
            active_access_point = null;
        }

        var ap = device.active_access_point;
        if (ap != null) {
            active_access_point = _access_points.get(ap.bssid);
            on_active_access_point_notify();
            ap_handler = active_access_point.notify.connect(on_active_access_point_notify);
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
        if (device.active_connection == null)
            return false;

        var conn = device.active_connection.connection;
        if (conn == null)
            return false;

        var ip4config = conn.get_setting_ip4_config();
        if (ip4config == null)
            return false;

        return ip4config.method == NM.SettingIP4Config.METHOD_SHARED;
    }
}
