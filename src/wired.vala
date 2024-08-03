public class AstalNetwork.Wired : Object {
    private const string ICON_CONNECTED = "network-wired-symbolic";
    private const string ICON_DISCONNECTED = "network-wired-disconnected-symbolic";
    private const string ICON_ACQUIRING = "network-wired-acquiring-symbolic";
    private const string ICON_NO_ROUTE = "network-wired-no-route-symbolic";

    public NM.DeviceEthernet device { get; construct set; }

    public NM.ActiveConnection connection;
    private ulong connection_handler = 0;

    internal Wired(NM.DeviceEthernet device) {
        this.device = device;

        speed = device.speed;
        state = (DeviceState)device.state;
        icon_name = _icon();

        device.notify.connect((pspec) => {
            if (pspec.name == "speed") {
                speed = device.speed;
            }
            if (pspec.name == "state") {
                state = (DeviceState)device.state;
            }
            if (pspec.name == "active-connection") {
                on_active_connection();
            }
            icon_name = _icon();
        });

        device.client.notify.connect(() => { icon_name = _icon(); });

        on_active_connection();
        icon_name = _icon();
    }

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
