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

    public GenericArray<NM.RemoteConnection> get_connections() {
        return (GenericArray<NM.RemoteConnection>)ap.filter_connections(
            wifi.device.client.connections
        );
    }

    public string get_path() {
        return ap.get_path();
    }

    public bool requires_password {
        get {
            return wpa_flags != NM.80211ApSecurityFlags.NONE ||
                rsn_flags != NM.80211ApSecurityFlags.NONE;
        }
    }

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
    }

    /**
     * Activates the first connection associated with this AccessPoint
     * or creates a new SimpleConnection using "wpa-psk" and activates it.
     * Returns whether the connection is the new active connection. 
     */
    public async void activate(string? password = null) throws Error {
        var conns = get_connections();

        if (conns.length > 0) {
            var first_conn = conns.get(0);

            if (password != null) {
                var security = first_conn.get_setting_wireless_security();
                security.psk = password;
                yield first_conn.commit_changes_async(true, null);
            }

            yield ap.client.activate_connection_async(
                first_conn,
                wifi.device,
                get_path(),
                null
            );
        } else {
            var connection = NM.SimpleConnection.new();

            connection.add_setting(new NM.SettingWireless() {
                ssid = this.ap.ssid,
                bssid = this.bssid,
            });

            connection.add_setting(new NM.SettingWirelessSecurity() {
                key_mgmt = "wpa-psk",
                psk = password,
            });

            yield ap.client.add_and_activate_connection_async(
                connection,
                wifi.device,
                get_path(),
                null
            );
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
