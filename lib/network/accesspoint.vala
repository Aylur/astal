public class AstalNetwork.AccessPoint : Object {
    private Wifi wifi;
    private NM.AccessPoint ap;

    public uint bandwidth { get { return ap.bandwidth; } }
    public string bssid { owned get { return ap.bssid; } }
    public uint frequency { get { return ap.frequency; } }
    public int last_seen { get { return ap.last_seen; } }
    public uint max_bitrate { get { return ap.max_bitrate; } }
    public uint8 strength { get { return ap.strength; } }
    public string icon_name { get; private set; }
    public NM.80211Mode mode { get { return ap.mode; } }
    public NM.80211ApFlags flags { get { return ap.flags; } }
    public NM.80211ApSecurityFlags rsn_flags { get { return ap.rsn_flags; } }
    public NM.80211ApSecurityFlags wpa_flags { get { return ap.wpa_flags; } }
    public bool hidden { get { return ssid == "Unknown" || ssid == null; } }
    public bool requires_password { get { return wpa_flags != NM.80211ApSecurityFlags.NONE || rsn_flags != NM.80211ApSecurityFlags.NONE; } }
    public bool saved { get; private set; }
    public string uuid { get; private set; }

    public string? ssid {
        owned get {
            if (ap.ssid == null)
                return null;

            return (string)NM.Utils.ssid_to_utf8(ap.ssid.get_data());
        }
    }

    internal AccessPoint(Wifi wifi, NM.AccessPoint ap) {
        this.wifi = wifi;
        this.ap = ap;
        ap.notify.connect((pspec) => {
            if (get_class().find_property(pspec.name) != null)
                notify_property(pspec.name);
            if (pspec.name == "strength")
                icon_name = _icon();
        });
        icon_name = _icon();
        set_attrs();
    }

    private NM.Connection? find_saved_connection_by_ssid(string ssid) {
        foreach (var connection in ap.client.get_connections()) {
            var wireless_setting = connection.get_setting_wireless();
            if (wireless_setting == null) {
                continue;
            }

            var connection_ssid_data = wireless_setting.get_ssid();
            if (connection_ssid_data == null) {
                continue;
            }

            var connection_ssid = NM.Utils.ssid_to_utf8(connection_ssid_data.get_data());
            if (connection_ssid == ssid) {
                return connection;
            }
        }
        return null;
    }

    private async void activate_saved_connection(NM.Connection saved_connection) throws Error {
        try {
            yield ap.client.activate_connection_async(saved_connection, wifi.device, ap.get_path(), null);
        } catch (Error err) {
            critical(err.message);
        }
    }

    private async void create_new_connection(string? password = null) throws Error {
        var connection = NM.SimpleConnection.new();
        var setting = new NM.SettingWireless();

        setting.set_property("ssid", ssid);
        setting.set_property("bssid", bssid);
        setting.set_property("mode", "infrastructure");
        connection.add_setting(setting);

        if (password != null) {
            var setting_wireless_security = new NM.SettingWirelessSecurity();
            setting_wireless_security.set_property("key-mgmt", "wpa-psk");
            setting_wireless_security.set_property("psk", password);
            connection.add_setting(setting_wireless_security);
        }

        yield ap.client.add_and_activate_connection_async(connection, wifi.device, ap.get_path(), null);
    }

    public async void connect_to_ap(string? password = null) {
        try {
            var saved_connection = find_saved_connection_by_ssid(ssid);

            if (saved_connection != null) {
                yield activate_saved_connection(saved_connection);
            }
            else {
                yield create_new_connection(password);
            }
        } catch (Error e) {
            critical(e.message);
        }
    }

    public async void forget_ap() throws Error {
        try{
            var connection = (NM.RemoteConnection?) null;
            foreach (var conn in ap.client.get_connections()) {
                if (conn.get_uuid() == uuid) {
                    connection = conn;
                    break;
                }
            }

            if (connection == null) {
                critical("Connection with UUID %s not found.".printf(uuid));
            }

            yield connection.delete_async(null);
        } catch (Error e) {
            critical(e.message);
        }
    }

    public async void disconnect_from_ap() {
        try {
            var active_connection = wifi.device.get_active_connection();

            if (active_connection == null) {
                critical("No active connection found.");
                return;
            }

            if (active_connection.uuid != uuid){
                critical("you are not connected to this access point.");
                return;
            }

            yield ap.client.deactivate_connection_async(active_connection, null);

        } catch (Error e) {
            critical(e.message);
        }
    }

    private void set_attrs(){
        if (ssid == null){
            saved = false;
            uuid = null;

            return;
        }

        var connection = find_saved_connection_by_ssid(ssid);
        saved = connection != null;
        uuid = null;
        if (connection != null){
            uuid = connection.get_uuid();
        }
    }

    private string _icon() {
        if (strength >= 80) return Wifi.ICON_EXCELLENT;
        if (strength >= 60) return Wifi.ICON_GOOD;
        if (strength >= 40) return Wifi.ICON_OK;
        if (strength >= 20) return Wifi.ICON_WEAK;
        return Wifi.ICON_NONE;
    }

    // TODO: connect to ap
    // public signal void auth();
    // public void try_connect(string? password) { }
}
