/**
 * Tracks the VPN connection profiles NetworkManager knows about.
 *
 * A VPN has no device of its own, so unlike {@link Wifi} and {@link Wired}
 * this is built on connection profiles instead.
 */
public class AstalNetwork.Vpn : Object {
    internal const string ICON_CONNECTED = "network-vpn-symbolic";
    internal const string ICON_ACQUIRING = "network-vpn-acquiring-symbolic";
    internal const string ICON_DISCONNECTED = "network-vpn-disabled-symbolic";

    private HashTable<string, VpnConnection> _connections =
        new HashTable<string, VpnConnection>(str_hash, str_equal);

    public NM.Client client { get; construct set; }

    public List<weak VpnConnection> connections {
        owned get { return _connections.get_values(); }
    }

    /** Whether any profile is fully connected. */
    public bool is_active { get; private set; }

    public string icon_name { get; private set; }

    public signal void connection_added(VpnConnection connection);
    public signal void connection_removed(VpnConnection connection);

    internal Vpn(NM.Client client) {
        this.client = client;

        foreach (var connection in client.connections) {
            add_connection(connection);
        }

        client.connection_added.connect(add_connection);
        client.connection_removed.connect(remove_connection);
        client.notify["active-connections"].connect(sync);

        sync();
    }

    /** Deactivates every active profile. */
    public async void deactivate_all() throws Error {
        foreach (var connection in _connections.get_values()) {
            yield connection.deactivate();
        }
    }

    /**
     * Whether this profile is one the user can toggle as a VPN.
     * WireGuard profiles are not of type "vpn", so both are matched.
     */
    internal static bool is_vpn(NM.RemoteConnection connection) {
        var setting = connection.get_setting_connection();
        if (setting == null) return false;

        // a port of a bond or a bridge is not a vpn of its own
        if (setting.get_controller() != null) return false;

        var type = setting.get_connection_type();
        return (type == NM.SettingVpn.SETTING_NAME)
            || (type == NM.SettingWireGuard.SETTING_NAME);
    }

    private void add_connection(NM.RemoteConnection connection) {
        if (!is_vpn(connection)) return;

        var uuid = connection.get_uuid();
        if ((uuid == null) || _connections.contains(uuid)) return;

        var vpn = new VpnConnection(client, connection);
        _connections.set(uuid, vpn);
        vpn.notify["state"].connect(update);

        connection_added(vpn);
        notify_property("connections");

        // NetworkManager emits connection-added after notify::active-connections,
        // so the active connection of a new profile has to be looked up again
        sync();
    }

    private void remove_connection(NM.RemoteConnection connection) {
        var uuid = connection.get_uuid();
        if (uuid == null) return;

        var vpn = _connections.get(uuid);
        if (vpn == null) return;

        _connections.remove(uuid);
        connection_removed(vpn);
        notify_property("connections");
        update();
    }

    private void sync() {
        foreach (var vpn in _connections.get_values()) {
            vpn.update_active_connection(null);
        }

        foreach (var active in client.get_active_connections()) {
            var connection = active.connection;
            if (connection == null) continue;

            var uuid = connection.get_uuid();
            if (uuid == null) continue;

            var vpn = _connections.get(uuid);
            if (vpn != null) vpn.update_active_connection(active);
        }

        update();
    }

    private void update() {
        var connected = false;
        var connecting = false;

        foreach (var vpn in _connections.get_values()) {
            if (vpn.state == NM.ActiveConnectionState.ACTIVATED) connected = true;
            if (vpn.state == NM.ActiveConnectionState.ACTIVATING) connecting = true;
        }

        is_active = connected;

        if (connected) {
            icon_name = ICON_CONNECTED;
        } else if (connecting) {
            icon_name = ICON_ACQUIRING;
        } else {
            icon_name = ICON_DISCONNECTED;
        }
    }
}

/**
 * A single VPN or WireGuard connection profile.
 */
public class AstalNetwork.VpnConnection : Object {
    private NM.Client client;
    private ulong state_handler = 0;

    public NM.RemoteConnection connection { get; construct set; }
    public NM.ActiveConnection? active_connection { get; private set; }

    public string id { owned get { return connection.get_id(); } }
    public string uuid { owned get { return connection.get_uuid(); } }

    public NM.ActiveConnectionState state { get; private set; }
    public string icon_name { get; private set; }

    public bool is_active {
        get { return state == NM.ActiveConnectionState.ACTIVATED; }
    }

    /**
     * Emitted when the profile stops for a reason other than the user
     * disconnecting it or cancelling the password prompt.
     */
    public signal void activation_failed(NM.ActiveConnectionStateReason reason);

    internal VpnConnection(NM.Client client, NM.RemoteConnection connection) {
        this.client = client;
        this.connection = connection;

        connection.changed.connect(() => {
            notify_property("id");
        });

        update();
    }

    public async void activate() throws Error {
        yield client.activate_connection_async(connection, null, null, null);
    }

    public async void deactivate() throws Error {
        if (active_connection == null) return;

        yield client.deactivate_connection_async(active_connection, null);
    }

    internal void update_active_connection(NM.ActiveConnection? active) {
        if (active == active_connection) return;

        if ((state_handler > 0) && (active_connection != null)) {
            active_connection.disconnect(state_handler);
            state_handler = 0;
        }

        active_connection = active;

        if (active_connection != null) {
            state_handler = active_connection.notify["state"].connect(on_state_changed);
        }

        update();
    }

    private void on_state_changed() {
        var previous = state;
        var current = active_connection.state;
        var reason = active_connection.get_state_reason();

        // update first, so a handler of the signal below reads a settled state
        update();

        // only on the edge into deactivated: NetworkManager can notify the
        // same state more than once, which would report one failure twice
        if ((current == NM.ActiveConnectionState.DEACTIVATED)
            && (previous != NM.ActiveConnectionState.DEACTIVATED)
            && (reason != NM.ActiveConnectionStateReason.NO_SECRETS)
            && (reason != NM.ActiveConnectionStateReason.USER_DISCONNECTED)) {
            activation_failed(reason);
        }
    }

    private void update() {
        state = active_connection == null
            ? NM.ActiveConnectionState.DEACTIVATED
            : active_connection.state;

        notify_property("is-active");
        icon_name = _icon();
    }

    private string _icon() {
        switch (state) {
            case NM.ActiveConnectionState.ACTIVATED:
                return Vpn.ICON_CONNECTED;
            case NM.ActiveConnectionState.ACTIVATING:
                return Vpn.ICON_ACQUIRING;
            default:
                return Vpn.ICON_DISCONNECTED;
        }
    }
}
