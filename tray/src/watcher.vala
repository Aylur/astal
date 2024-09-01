namespace AstalTray {
[DBus (name="org.kde.StatusNotifierWatcher")]
internal class StatusNotifierWatcher : Object {
    private HashTable<string, string> _items =
        new HashTable<string, string>(str_hash, str_equal);

    public string[] RegisteredStatusNotifierItems { owned get { return _items.get_values_as_ptr_array().data; } }
    public bool IsStatusNotifierHostRegistered { get; default = true; }
    public int ProtocolVersion { get; default = 0; }

    public signal void StatusNotifierItemRegistered(string service);
    public signal void StatusNotifierItemUnregistered(string service);
    public signal void StatusNotifierHostRegistered();
    public signal void StatusNotifierHostUnregistered();

    public void RegisterStatusNotifierItem(string service, BusName sender) throws DBusError, IOError {
        string busName;
        string path;
        if (service[0] == '/') {
            path = service;
            busName = sender;
        } else {
            busName = service;
            path = "/StatusNotifierItem";
        }

        Bus.get_sync(BusType.SESSION).signal_subscribe(
            null,
            "org.freedesktop.DBus",
            "NameOwnerChanged",
            null,
            null,
            DBusSignalFlags.NONE,
            (connection, sender_name, path, interface_name, signal_name, parameters) => {
                string name = null;
                string new_owner = null;
                string old_owner = null;
                parameters.get("(sss)", &name, &old_owner, &new_owner);
                if (new_owner == "" && _items.contains(old_owner)) {
                    string full_path = _items.take(old_owner);
                    StatusNotifierItemUnregistered(full_path);
                }
            }
        );

        _items.set(busName, busName+path);
        StatusNotifierItemRegistered(busName+path);
    }

    public void RegisterStatusNotifierHost(string service) throws DBusError, IOError {
        /* NOTE:
            usually the watcher should keep track of registered host
            but some tray applications do net register their trayitem properly
            when hosts register/deregister. This is fixed by setting isHostRegistered
            always to true, this also make host handling logic unneccessary.
         */
    }
}
}
