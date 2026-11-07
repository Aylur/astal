public class AstalNetwork.AccessPoint : Object {
    private Wifi wifi;
    public NM.AccessPoint ap;

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

    public GenericArray<NM.RemoteConnection> get_connections() {
        return (GenericArray<NM.RemoteConnection>)ap.filter_connections(ap.client.connections);
    }

    public string get_path() {
        return ap.get_path();
    }

    /**
     * Whether this is the access point the wifi device is connected to.
     */
    public bool active {
        get { return wifi.active_access_point == this; }
    }
    private bool was_active = false;

    /**
     * Whether connecting needs a password: WPA/WPA2/WPA3 personal, WEP and enterprise networks.
     */
    public bool requires_password {
        get {
            var security = wpa_flags | rsn_flags;

            // privacy without wpa/rsn means wep
            if (security == NM.80211ApSecurityFlags.NONE) {
                return NM.80211ApFlags.PRIVACY in flags;
            }

            var with_password =
                NM.80211ApSecurityFlags.KEY_MGMT_PSK |
                NM.80211ApSecurityFlags.KEY_MGMT_SAE |
                NM.80211ApSecurityFlags.KEY_MGMT_802_1X |
                NM.80211ApSecurityFlags.KEY_MGMT_EAP_SUITE_B_192;

            return (security & with_password) != 0;
        }
    }

    public string? ssid {
        owned get {
            if (ap.ssid == null) return null;

            return (string)NM.Utils.ssid_to_utf8(ap.ssid.get_data());
        }
    }

    internal AccessPoint(Wifi wifi, NM.AccessPoint ap) {
        this.wifi = wifi;
        this.ap = ap;

        ap.notify.connect(on_ap_notify);

        was_active = active;
        wifi.notify["active-access-point"].connect(on_active_access_point);

        icon_name = _icon();
    }

    private void on_ap_notify(Object obj, ParamSpec pspec) {
        if (get_class().find_property(pspec.name) != null) {
            notify_property(pspec.name);
        }
        if (pspec.name == "strength") icon_name = _icon();
        if ((pspec.name == "flags") || (pspec.name == "wpa-flags") || (pspec.name == "rsn-flags")) {
            notify_property("requires-password");
        }
    }

    private void on_active_access_point() {
        if (was_active != active) {
            was_active = active;
            notify_property("active");
        }
    }

    /**
     * Activates the first saved connection of this access point,
     * or creates and activates a new one if there is none.
     *
     * When a password is given it is saved to the connection,
     * it is ignored for open networks.
     * Enterprise (802.1X) networks need an already configured connection.
     */
    public async void activate(string? password = null) throws Error {
        var device = wifi.device;
        if (device == null) {
            throw new IOError.NOT_FOUND("no wifi device");
        }

        var conns = get_connections();

        if (conns.length > 0) {
            var conn = conns.get(0);
            var security = conn.get_setting_wireless_security();

            if ((password != null) && (security != null) && set_secret(security, password)) {
                yield conn.commit_changes_async(true, null);
            }

            yield ap.client.activate_connection_async(conn, device, get_path(), null);
        } else {
            var connection = NM.SimpleConnection.new();

            connection.add_setting(new NM.SettingWireless() { ssid = ap.ssid });

            var key_mgmt = get_key_mgmt();
            if (key_mgmt != null) {
                var security = new NM.SettingWirelessSecurity() {
                    key_mgmt = key_mgmt,
                };

                if (password != null) set_secret(security, password);
                connection.add_setting(security);
            }

            yield ap.client.add_and_activate_connection_async(connection, device, get_path(), null);
        }
    }

    private string? get_key_mgmt() throws Error {
        var security = wpa_flags | rsn_flags;

        if (
            (NM.80211ApSecurityFlags.KEY_MGMT_802_1X in security) ||
            (NM.80211ApSecurityFlags.KEY_MGMT_EAP_SUITE_B_192 in security)
        ) {
            throw new IOError.NOT_SUPPORTED("enterprise networks need a configured connection");
        }

        if (NM.80211ApSecurityFlags.KEY_MGMT_PSK in security) return "wpa-psk";
        if (NM.80211ApSecurityFlags.KEY_MGMT_SAE in security) return "sae";
        if (NM.80211ApSecurityFlags.KEY_MGMT_OWE in security) return "owe";

        if ((security == NM.80211ApSecurityFlags.NONE) && (NM.80211ApFlags.PRIVACY in flags)) {
            return "none";
        }

        return null;
    }

    // returns whether the key-mgmt takes a password
    private bool set_secret(NM.SettingWirelessSecurity security, string password) {
        switch (security.key_mgmt) {
            case "wpa-psk":
            case "sae":
                security.psk = password;
                return true;
            case "none":
                security.wep_key_type = NM.Utils.wep_key_valid(password, NM.WepKeyType.KEY)
                    ? NM.WepKeyType.KEY
                    : NM.WepKeyType.PASSPHRASE;
                security.set_wep_key(0, password);
                return true;
            default:
                return false;
        }
    }

    private string _icon() {
        if (strength >= 80) return Wifi.ICON_EXCELLENT;
        if (strength >= 60) return Wifi.ICON_GOOD;
        if (strength >= 40) return Wifi.ICON_OK;
        if (strength >= 20) return Wifi.ICON_WEAK;
        return Wifi.ICON_NONE;
    }
}
