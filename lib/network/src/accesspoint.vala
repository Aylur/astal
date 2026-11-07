/**
 * Errors thrown when an activation fails after NetworkManager accepted it.
 */
public errordomain AstalNetwork.ActivationError {
    /** The password is missing or wrong. */
    AUTH_FAILED,
    /** The connection attempt timed out. */
    TIMEOUT,
    /** Any other reason, the message contains the NetworkManager reason. */
    FAILED,
}

/**
 * A wifi access point visible to the [class@AstalNetwork.Wifi] device.
 */
public class AstalNetwork.AccessPoint : Object {
    private Wifi wifi;

    /**
     * The underlying NetworkManager access point.
     */
    public NM.AccessPoint ap;

    /**
     * The channel bandwidth in MHz.
     */
    public uint bandwidth { get { return ap.bandwidth; } }

    /**
     * The hardware address of the access point.
     */
    public string bssid { owned get { return ap.bssid; } }

    /**
     * The frequency in MHz.
     */
    public uint frequency { get { return ap.frequency; } }

    /**
     * When the access point was last seen in a scan,
     * in seconds of `CLOCK_BOOTTIME`, or -1 if it was never seen.
     */
    public int last_seen { get { return ap.last_seen; } }

    /**
     * The maximum bitrate in kb/s.
     */
    public uint max_bitrate { get { return ap.max_bitrate; } }

    /**
     * The signal strength in percent.
     */
    public uint8 strength { get { return ap.strength; } }

    /**
     * Icon name representing the signal strength.
     */
    public string icon_name { get; private set; }

    /**
     * The mode of the access point, such as infrastructure or ad-hoc.
     */
    public NM.80211Mode mode { get { return ap.mode; } }

    /**
     * The capabilities of the access point.
     */
    public NM.80211ApFlags flags { get { return ap.flags; } }

    /**
     * The WPA2/WPA3 (RSN) security capabilities.
     */
    public NM.80211ApSecurityFlags rsn_flags { get { return ap.rsn_flags; } }

    /**
     * The WPA security capabilities.
     */
    public NM.80211ApSecurityFlags wpa_flags { get { return ap.wpa_flags; } }

    /**
     * Get the saved connections that can be used with this access point.
     */
    public GenericArray<NM.RemoteConnection> get_connections() {
        return (GenericArray<NM.RemoteConnection>)ap.filter_connections(ap.client.connections);
    }

    /**
     * Get the DBus object path of the access point.
     */
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

    /**
     * The name of the network.
     */
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
     * When a password is given it is saved to the connection,
     * it is ignored for open networks.
     * Enterprise (802.1X) networks need an already configured connection.
     * Resolves once the connection is activated and throws an
     * [error@AstalNetwork.ActivationError] if the activation fails.
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

            var ac = yield ap.client.activate_connection_async(conn, device, get_path(), null);
            yield wait_for_activation(ac);
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

            var ac = yield ap.client.add_and_activate_connection_async(
                connection,
                device,
                get_path(),
                null
            );
            yield wait_for_activation(ac);
        }
    }

    private async void wait_for_activation(NM.ActiveConnection ac) throws Error {
        if (!is_final(ac.state)) {
            ulong state_handler = 0;
            state_handler = ac.state_changed.connect((state) => {
                if (is_final((NM.ActiveConnectionState)state)) {
                    SignalHandler.disconnect(ac, state_handler);
                    Idle.add(wait_for_activation.callback);
                }
            });
            yield;
        }

        if (ac.state == NM.ActiveConnectionState.ACTIVATED) return;

        var reason = ac.get_state_reason();
        var reasons = (EnumClass)typeof(NM.ActiveConnectionStateReason).class_ref();
        var nick = reasons.get_value(reason).value_nick;

        switch (reason) {
            case NM.ActiveConnectionStateReason.NO_SECRETS:
            case NM.ActiveConnectionStateReason.LOGIN_FAILED:
                throw new ActivationError.AUTH_FAILED(nick);
            case NM.ActiveConnectionStateReason.CONNECT_TIMEOUT:
            case NM.ActiveConnectionStateReason.SERVICE_START_TIMEOUT:
                throw new ActivationError.TIMEOUT(nick);
            default:
                throw new ActivationError.FAILED(nick);
        }
    }

    private static bool is_final(NM.ActiveConnectionState state) {
        return (state == NM.ActiveConnectionState.ACTIVATED) ||
        (state == NM.ActiveConnectionState.DEACTIVATING) ||
        (state == NM.ActiveConnectionState.DEACTIVATED);
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
