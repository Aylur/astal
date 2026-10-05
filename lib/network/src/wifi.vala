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

    private GenericArray<AccessPoint> _access_points = new GenericArray<AccessPoint>();

    public NM.DeviceWifi device { get; construct set; }

    public NM.ActiveConnection? active_connection { get; private set; }
    private ulong connection_handler = 0;

    public AccessPoint? active_access_point { get; private set; }
    private NM.AccessPoint? active_nm_ap = null;
    private ulong ap_handler = 0;

    public List<weak AccessPoint> access_points {
        owned get {
            var list = new List<weak AccessPoint>();
            foreach (var ap in _access_points) {
                list.append(ap);
            }
            return list;
        }
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
    public string? ssid { get; private set; }
    public bool is_hotspot { get; private set; }
    public bool scanning { get; private set; }

    public signal void access_point_added(AccessPoint ap) ;
    public signal void access_point_removed(AccessPoint ap) ;

    internal Wifi(NM.DeviceWifi device) {
        this.device = device;

        foreach (var ap in device.access_points) {
            add_access_point(ap);
        }

        device.access_point_added.connect((access_point) => {
            add_access_point((NM.AccessPoint)access_point);
        });

        device.access_point_removed.connect((access_point) => {
            remove_access_point((NM.AccessPoint)access_point);
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

    private void add_access_point(NM.AccessPoint ap) {
        if (ap.ssid == null) {
            // NetworkManager creates the AccessPoint before it knows the ssid.
            // Adding it now would list it as a nameless network, so wait for
            // the ssid to arrive. A hidden ap never gets one and stays out.
            ulong id = 0;
            id = ap.notify["ssid"].connect(() => {
                if (ap.ssid == null) return;
                ap.disconnect(id);
                add_access_point(ap);
            });
            return;
        }

        // one router advertises one ap per radio and a mesh one per node.
        // they are one network to the user, so group them.
        foreach (var group in _access_points) {
            if (group.matches(ap)) {
                group.add(ap);
                resolve_active_access_point();
                return;
            }
        }

        var new_ap = new AccessPoint(this, ap);
        _access_points.add(new_ap);
        access_point_added(new_ap);
        notify_property("access-points");
        resolve_active_access_point();
    }

    private void remove_access_point(NM.AccessPoint ap) {
        for (var i = 0; i < _access_points.length; ++i) {
            var group = _access_points.get(i);
            // an ap that never got an ssid was never added to any group
            if (!group.remove(ap)) continue;

            if (group.is_empty) {
                _access_points.remove_index(i);
                access_point_removed(group);
                notify_property("access-points");
            }
            return;
        }
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
        if (active_nm_ap == null) return;

        bandwidth = active_nm_ap.bandwidth;
        frequency = active_nm_ap.frequency;
        strength = active_nm_ap.strength;
        ssid = (active_nm_ap.ssid == null)
            ? null
            : (string)NM.Utils.ssid_to_utf8(active_nm_ap.ssid.get_data());
    }

    private void on_active_access_point() {
        if ((ap_handler > 0) && (active_nm_ap != null)) {
            active_nm_ap.disconnect(ap_handler);
            ap_handler = 0;
        }

        active_nm_ap = device.active_access_point;
        resolve_active_access_point();
        on_active_access_point_notify();

        if (active_nm_ap != null) {
            ap_handler = active_nm_ap.notify.connect(on_active_access_point_notify);
        }
    }

    // points active_access_point at the group holding the active ap.
    // the group can appear after the active ap does, because an ap without
    // an ssid yet waits before it joins one.
    private void resolve_active_access_point() {
        if (active_nm_ap == null) {
            active_access_point = null;
            return;
        }

        foreach (var group in _access_points) {
            if (group.contains(active_nm_ap)) {
                active_access_point = group;
                return;
            }
        }

        active_access_point = null;
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
