public class AstalNetwork.ConnectionProfile : Object {
    private Wifi wifi;
    private NM.AccessPoint ap;
    private NM.RemoteConnection connection;

    public string? password { get; private set; }
    public string uuid { get { return connection.get_uuid(); } }
    public bool is_active { get { return uuid == wifi.device.get_active_connection().uuid; }}

    public ConnectionProfile(NM.RemoteConnection connection, string? password = null, Wifi wifi, NM.AccessPoint ap) {
        this.ap = ap;
        this.connection = connection;
        this.wifi = wifi;
        this.password = password;
    }

    public static ConnectionProfile? fromNMConnection(NM.RemoteConnection connection, Wifi wifi, NM.AccessPoint ap) {
        var wireless_setting = connection.get_setting_wireless();
        if (wireless_setting == null) {
            return null;
        }

        var connection_ssid_data = wireless_setting.get_ssid();
        if (connection_ssid_data == null) {
            return null;
        }

        string? password = null;
        var security_setting = connection.get_setting_wireless_security();
        if (security_setting != null) {
            password = security_setting.psk;
        }

        return new ConnectionProfile(connection, password, wifi, ap);
    }


    public async void activate_connection() throws Error {
        try {
            yield ap.client.activate_connection_async(connection, wifi.device, ap.get_path(), null);
        } catch (Error err) {
            critical(err.message);
        }
    }

    public async void deactivate_connection() {
        try {
            var active_connection = wifi.device.get_active_connection();

            if (active_connection == null) {
                critical("No active connection found.");
                return;
            }

            if (!is_active){
                critical("you are not connected to this profile.");
                return;
            }

            yield ap.client.deactivate_connection_async(active_connection, null);
        } catch (Error e) {
            critical(e.message);
        }
    }

    public async void delete_profile() throws Error {
        try{
            yield connection.delete_async(null);
        } catch (Error e) {
            critical(e.message);
        }
    }


    // TODO: -> Create update profile fun

}
