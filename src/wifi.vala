public class AstalNetwork.Wifi : Object {
    private const string ICON_EXCELLENT = "network-wireless-signal-excellent-symbolic";
    private const string ICON_OK = "network-wireless-signal-ok-symbolic";
    private const string ICON_GOOD = "network-wireless-signal-good-symbolic";
    private const string ICON_WEAK = "network-wireless-signal-weak-symbolic";
    private const string ICON_NONE = "network-wireless-signal-none-symbolic";
    private const string ICON_ACQUIRING = "network-wireless-acquiring-symbolic";
    private const string ICON_CONNECTED = "network-wireless-connected-symbolic";
    private const string ICON_DISABLED = "network-wireless-disabled-symbolic";
    private const string ICON_OFFLINE = "network-wireless-offline-symbolic";
    private const string ICON_NO_ROUTE = "network-wireless-no-route-symbolic";

    private NM.Client client;
    public NM.DeviceWifi device { get; private set; }

    public NM.ActiveConnection? connection;
    private ulong connection_handler = 0;

    public NM.AccessPoint? access_point;
    private ulong ap_handler = 0;

    internal Wifi(NM.DeviceWifi device, NM.Client client) {
        this.device = device;
        this.client = client;

        on_active_connection();
        device.notify["active-connection"].connect(on_active_connection);

        on_active_access_point();
        device.notify["active-access-point"].connect(on_active_access_point);

        state = (DeviceState)device.state;
        device.access_point_added.connect(() => notify_property("access-points"));
        device.access_point_removed.connect(() => notify_property("access-points"));
        client.notify["wireless-enabled"].connect(() => notify_property("enabled"));
        device.state_changed.connect((n, o, r) => {
            state_changed(n, o, r);
            state = (DeviceState)n;
        });

        device.notify.connect(() => { icon_name = _icon(); });
        client.notify.connect(() => { icon_name = _icon(); });
        icon_name = _icon();
    }

    public signal void state_changed(
        DeviceState new_state,
        DeviceState old_state,
        NM.DeviceStateReason reaseon
    );

    private void on_active_connection() {
        if (connection_handler > 0 && connection != null) {
            connection.disconnect(connection_handler);
            connection_handler = 0;
            connection = null;
        }

        connection = device.active_connection;
        if (connection != null) {
            connection_handler = connection.notify["state"].connect(() => {
                internet = Internet.from_device(device);
            });
        }
    }

    private void on_active_access_point_notify() {
        bandwidth = access_point.bandwidth;
        frequency = access_point.frequency;
        strength = access_point.strength;
        ssid = access_point.ssid;
    }

    private void on_active_access_point() {
        if (ap_handler > 0 && access_point != null) {
            access_point.disconnect(ap_handler);
            ap_handler = 0;
            access_point = null;
        }

        access_point = device.active_access_point;
        if (access_point != null) {
            on_active_access_point_notify();
            ap_handler = access_point.notify.connect(on_active_access_point_notify);
        }
    }

    public bool enabled {
        get { return client.wireless_enabled; }
        set { client.wireless_enabled = value; }
    }

    public GenericArray<NM.AccessPoint> access_points {
        get { return device.access_points; }
    }

    public Internet internet { get; private set; }
    public uint bandwidth { get; private set; }
    public Bytes ssid { get; private set; }
    public uint8 strength { get; private set; }
    public uint frequency { get; private set; }
    public DeviceState state { get; private set; }
    public string icon_name { get; private set; }

    private string _icon() {
        var full = client.connectivity == NM.ConnectivityState.FULL;

        if (!enabled) return ICON_DISABLED;

        if (internet == Internet.CONNECTED) {
            if (!full) return ICON_NO_ROUTE;

            if (strength >= 80) return ICON_EXCELLENT;
            if (strength >= 60) return ICON_GOOD;
            if (strength >= 40) return ICON_OK;
            if (strength >= 20) return ICON_WEAK;
            if (strength >= 0) return ICON_NONE;

            return ICON_CONNECTED;
        }

        if (internet == Internet.CONNECTING) {
            return ICON_ACQUIRING;
        }

        return ICON_OFFLINE;
    }
}

namespace AstalNetwork {
    public string ssid_to_string(Bytes? bytes) {
        if (bytes == null)
            return "";

        return (string)NM.Utils.ssid_to_utf8(bytes.get_data());
    }
}
