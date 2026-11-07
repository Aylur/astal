public class AstalNetwork.Wired : Object {
    private const string ICON_CONNECTED = "network-wired-symbolic";
    private const string ICON_DISCONNECTED = "network-wired-disconnected-symbolic";
    private const string ICON_ACQUIRING = "network-wired-acquiring-symbolic";
    private const string ICON_NO_ROUTE = "network-wired-no-route-symbolic";

    private NM.Client client;

    private NM.DeviceEthernet? _device;
    public NM.DeviceEthernet? device {
        get { return _device; }
    }

    public NM.ActiveConnection? active_connection { get; private set; }
    private ulong connection_handler = 0;

    private ulong[] device_handlers = {};

    internal Wired(NM.Client client) {
        this.client = client;

        client.notify.connect(() => {
            icon_name = _icon();
        });
        icon_name = _icon();
    }

    internal void set_device(NM.DeviceEthernet? device) {
        if (_device == device) return;
        if (_device != null) unset_device();

        _device = device;
        notify_property("device");

        if (device == null) {
            icon_name = _icon();
            return;
        }

        speed = device.speed;
        state = (DeviceState)device.state;
        icon_name = _icon();

        device_handlers += device.notify.connect((pspec) => {
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

        on_active_connection();
        icon_name = _icon();
    }

    private void unset_device() {
        foreach (var id in device_handlers) {
            SignalHandler.disconnect(_device, id);
        }
        device_handlers = {};

        if (connection_handler > 0) {
            active_connection.disconnect(connection_handler);
            connection_handler = 0;
        }
        active_connection = null;

        internet = Internet.DISCONNECTED;
        state = DeviceState.UNKNOWN;
        speed = 0;
    }

    private void on_active_connection() {
        if ((connection_handler > 0) && (active_connection != null)) {
            active_connection.disconnect(connection_handler);
            connection_handler = 0;
            active_connection = null;
        }

        active_connection = device.active_connection;
        if (active_connection != null) {
            connection_handler = active_connection.notify["state"].connect(() => {
                internet = Internet.from_device(device);
                icon_name = _icon();
            });
        }

        internet = Internet.from_device(device);
    }

    public uint speed { get; private set; }
    public Internet internet { get; private set; }
    public DeviceState state { get; private set; }
    public string icon_name { get; private set; }

    private string _icon() {
        var full = client.connectivity == NM.ConnectivityState.FULL;

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
