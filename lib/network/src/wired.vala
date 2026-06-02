public class AstalNetwork.Wired : Object {
    private const string ICON_CONNECTED = "network-wired-symbolic";
    private const string ICON_DISCONNECTED = "network-wired-disconnected-symbolic";
    private const string ICON_ACQUIRING = "network-wired-acquiring-symbolic";
    private const string ICON_NO_ROUTE = "network-wired-no-route-symbolic";

    public NM.DeviceEthernet device { get; construct set; }

    public NM.ActiveConnection connection;
    private ulong connection_handler = 0;
    private ulong device_active_connection_handler = 0;
    private ulong device_speed_handler = 0;
    private ulong device_state_handler = 0;
    private ulong client_connectivity_handler = 0;

    internal Wired(NM.DeviceEthernet device) {
        this.device = device;

        speed = device.speed;
        state = (DeviceState)device.state;
        icon_name = _icon();

        device_speed_handler = device.notify["speed"].connect(() => { speed = device.speed; });
        device_state_handler =
            device.notify["state"].connect(() => { state = (DeviceState)device.state; });
        device_active_connection_handler =
            device.notify["active-connection"].connect(on_active_connection);

        client_connectivity_handler =
            device.client.notify["connectivity"].connect(() => { icon_name = _icon(); });

        on_active_connection();
        icon_name = _icon();
    }

    private void on_active_connection() {
        if ((connection_handler > 0) && (connection != null)) {
            SignalHandler.disconnect(connection, connection_handler);
            connection_handler = 0;
            connection = null;
        }

        connection = device.active_connection;
        internet = Internet.from_device(device);
        if (connection != null) {
            connection_handler = connection.notify["state"].connect(() => {
                internet = Internet.from_device(device);
                icon_name = _icon();
            });
        }
        icon_name = _icon();
    }

    internal void disconnect_signals() {
        if ((connection_handler > 0) && (connection != null)) {
            SignalHandler.disconnect(connection, connection_handler);
            connection_handler = 0;
        }
        connection = null;

        if (device_active_connection_handler > 0) {
            SignalHandler.disconnect(device, device_active_connection_handler);
            device_active_connection_handler = 0;
        }

        if (device_speed_handler > 0) {
            SignalHandler.disconnect(device, device_speed_handler);
            device_speed_handler = 0;
        }

        if (device_state_handler > 0) {
            SignalHandler.disconnect(device, device_state_handler);
            device_state_handler = 0;
        }

        if (client_connectivity_handler > 0) {
            SignalHandler.disconnect(device.client, client_connectivity_handler);
            client_connectivity_handler = 0;
        }
    }

    public uint speed { get; private set; }
    public Internet internet { get; private set; }
    public DeviceState state { get; private set; }
    public string icon_name { get; private set; }

    private string _icon() {
        var full = device.client.connectivity == NM.ConnectivityState.FULL;

        if (internet == Internet.CONNECTING) {
            return ICON_ACQUIRING;
        }

        if (internet == Internet.CONNECTED) {
            if (!full) return ICON_NO_ROUTE;

            return ICON_CONNECTED;
        }

        return ICON_DISCONNECTED;
    }
}
