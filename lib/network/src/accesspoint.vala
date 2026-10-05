/**
 * A wifi network, as one or more NM.AccessPoint that share
 * an ssid, a mode and a security type.
 *
 * A router with a 2.4GHz and a 5GHz radio, or a mesh with several nodes,
 * advertises one NM.AccessPoint for each radio. They are one network to
 * the user, so this groups them and reads through to the strongest one.
 */
public class AstalNetwork.AccessPoint : Object {
    private Wifi wifi;
    private NM.AccessPoint[] aps = {};
    private ulong[] handlers = {};

    /** The strongest NM.AccessPoint of this network. */
    public NM.AccessPoint ap { get; private set; }

    public uint bandwidth { get { return ap.bandwidth; } }
    public string? bssid { owned get { return ap.bssid; } }
    public uint frequency { get { return ap.frequency; } }
    public int last_seen { get { return ap.last_seen; } }
    public uint max_bitrate { get { return ap.max_bitrate; } }
    public uint8 strength { get { return ap.strength; } }
    public string icon_name { get; private set; }
    public NM.80211Mode mode { get { return ap.mode; } }
    public NM.80211ApFlags flags { get { return ap.flags; } }
    public NM.80211ApSecurityFlags rsn_flags { get { return ap.rsn_flags; } }
    public NM.80211ApSecurityFlags wpa_flags { get { return ap.wpa_flags; } }

    /** How many NM.AccessPoint advertise this network. */
    public uint access_point_count { get { return aps.length; } }

    /**
     * Security type of this network, as negotiated between the capabilities
     * the access points advertise and those of the wifi device.
     */
    public NM.Utils.SecurityType security { get; private set; }

    public GenericArray<NM.RemoteConnection> get_connections() {
        return (GenericArray<NM.RemoteConnection>)ap.filter_connections(
            wifi.device.client.connections
        );
    }

    public string get_path() {
        return ap.get_path();
    }

    /**
     * Whether {@link activate} needs a password for this network.
     *
     * Note that enterprise networks need more than a password,
     * so this is `false` for them. See {@link security}.
     */
    public bool requires_password {
        get {
            switch (security) {
                case NM.Utils.SecurityType.STATIC_WEP:
                case NM.Utils.SecurityType.WPA_PSK:
                case NM.Utils.SecurityType.WPA2_PSK:
                case NM.Utils.SecurityType.SAE:
                    return true;
                default:
                    return false;
            }
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
        this.security = security_type(wifi.device, ap);

        add(ap);
    }

    /**
     * Whether the given NM.AccessPoint advertises this same network.
     */
    internal bool matches(NM.AccessPoint other) {
        if ((other.ssid == null) || (ap.ssid == null)) return false;

        return ap.ssid.compare(other.ssid) == 0
            && ap.mode == other.mode
            && security == security_type(wifi.device, other);
    }

    internal bool contains(NM.AccessPoint other) {
        return index_of(other) >= 0;
    }

    internal bool is_empty {
        get { return aps.length == 0; }
    }

    internal void add(NM.AccessPoint member) {
        if (contains(member)) return;

        aps += member;
        handlers += member.notify.connect((pspec) => {
            if (pspec.name == "strength") update_best();
            if (member != ap) return;

            if (get_class().find_property(pspec.name) != null) notify_property(pspec.name);
            if (pspec.name == "strength") icon_name = _icon();
        });

        notify_property("access-point-count");
        update_best();
    }

    internal bool remove(NM.AccessPoint member) {
        var i = index_of(member);
        if (i < 0) return false;

        aps[i].disconnect(handlers[i]);
        for (var j = i; j < aps.length - 1; ++j) {
            aps[j] = aps[j + 1];
            handlers[j] = handlers[j + 1];
        }
        aps.resize(aps.length - 1);
        handlers.resize(handlers.length - 1);

        notify_property("access-point-count");
        update_best();
        return true;
    }

    private int index_of(NM.AccessPoint member) {
        for (var i = 0; i < aps.length; ++i) {
            if (aps[i] == member) return i;
        }
        return -1;
    }

    private void update_best() {
        NM.AccessPoint? best = null;
        foreach (var member in aps) {
            if ((best == null) || (member.strength > best.strength)) best = member;
        }

        // keep the last known ap when the group is emptied, so that a
        // consumer holding this object after removal still reads its ssid
        if ((best == null) || (best == ap)) {
            icon_name = _icon();
            return;
        }

        ap = best;

        // the whole read-through surface now comes from a different ap
        notify_property("bandwidth");
        notify_property("bssid");
        notify_property("frequency");
        notify_property("last-seen");
        notify_property("max-bitrate");
        notify_property("strength");
        notify_property("mode");
        notify_property("flags");
        notify_property("rsn-flags");
        notify_property("wpa-flags");
        icon_name = _icon();
    }

    /**
     * Activates the first connection associated with this network
     * or creates a new SimpleConnection matching its security type
     * and activates it.
     */
    public async void activate(string? password = null) throws Error {
        var conns = get_connections();

        if (conns.length > 0) {
            var first_conn = conns.get(0);

            if (password != null) {
                var security = first_conn.get_setting_wireless_security();
                if (security == null) {
                    var setting = security_setting(password);
                    if (setting != null) first_conn.add_setting(setting);
                } else {
                    apply_password(security, password);
                }
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

            // no bssid: pinning the connection to a single bssid stops
            // NetworkManager from roaming between the aps of this network
            connection.add_setting(new NM.SettingWireless() {
                ssid = this.ap.ssid,
            });

            var setting = security_setting(password);
            if (setting != null) connection.add_setting(setting);

            yield ap.client.add_and_activate_connection_async(
                connection,
                wifi.device,
                get_path(),
                null
            );
        }
    }

    /**
     * Returns the best security type the wifi device can use with this ap,
     * or `INVALID` when the device cannot connect to it at all.
     */
    internal static NM.Utils.SecurityType security_type(
        NM.DeviceWifi device,
        NM.AccessPoint ap
    ) {
        // ordered from strongest to weakest, the first match wins
        const NM.Utils.SecurityType[] TYPES = {
            NM.Utils.SecurityType.WPA3_SUITE_B_192,
            NM.Utils.SecurityType.SAE,
            NM.Utils.SecurityType.OWE,
            NM.Utils.SecurityType.WPA2_ENTERPRISE,
            NM.Utils.SecurityType.WPA2_PSK,
            NM.Utils.SecurityType.WPA_ENTERPRISE,
            NM.Utils.SecurityType.WPA_PSK,
            NM.Utils.SecurityType.DYNAMIC_WEP,
            NM.Utils.SecurityType.LEAP,
            NM.Utils.SecurityType.STATIC_WEP,
            NM.Utils.SecurityType.NONE,
        };

        var caps = device.wireless_capabilities;
        var adhoc = ap.mode == NM.80211Mode.ADHOC;

        foreach (var type in TYPES) {
            var valid = NM.Utils.security_valid(
                type, caps, true, adhoc, ap.flags, ap.wpa_flags, ap.rsn_flags
            );
            if (valid) return type;
        }

        return NM.Utils.SecurityType.INVALID;
    }

    private void apply_password(NM.SettingWirelessSecurity security, string password) {
        if (security.get_key_mgmt() == "none") {
            security.set_wep_key(0, password);
        } else {
            security.psk = password;
        }
    }

    private NM.SettingWirelessSecurity? security_setting(string? password) throws Error {
        switch (security) {
            case NM.Utils.SecurityType.NONE:
                return null;

            case NM.Utils.SecurityType.OWE:
                return new NM.SettingWirelessSecurity() { key_mgmt = "owe" };

            case NM.Utils.SecurityType.STATIC_WEP:
                return new NM.SettingWirelessSecurity() {
                    key_mgmt = "none",
                    wep_key_type = NM.WepKeyType.PASSPHRASE,
                    wep_key0 = password,
                };

            case NM.Utils.SecurityType.WPA_PSK:
            case NM.Utils.SecurityType.WPA2_PSK:
                return new NM.SettingWirelessSecurity() {
                    key_mgmt = "wpa-psk",
                    psk = password,
                };

            case NM.Utils.SecurityType.SAE:
                return new NM.SettingWirelessSecurity() {
                    key_mgmt = "sae",
                    psk = password,
                };

            default:
                // enterprise networks need an 802.1X setting which a password
                // alone cannot describe, so require an existing profile
                throw new IOError.NOT_SUPPORTED(
                    "cannot create a connection for \"%s\": " +
                    "define a profile for it first, for example with nm-connection-editor",
                    ssid ?? "unknown network"
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
