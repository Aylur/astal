namespace AstalTray {
[DBus (name="org.kde.StatusNotifierWatcher")]
internal class StatusNotifierWatcher : Object {
    private HashTable<string, string> _items =
        new HashTable<string, string>(str_hash, str_equal);
    private uint noc_signal_id;
    private DBusConnection bus;

    public string[] RegisteredStatusNotifierItems { owned get { return _items.get_values_as_ptr_array().data; } }
    public bool IsStatusNotifierHostRegistered { get; default = true; }
    public int ProtocolVersion { get; default = 0; }

    public signal void StatusNotifierItemRegistered(string service);
    public signal void StatusNotifierItemUnregistered(string service);
    public signal void StatusNotifierHostRegistered();
    public signal void StatusNotifierHostUnregistered();

    construct {
        try {
            bus = Bus.get_sync(BusType.SESSION);
            noc_signal_id = bus.signal_subscribe(
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
                    if (new_owner == "" && _items.contains(name)) {
                        string full_path = _items.take(name);
                        StatusNotifierItemUnregistered(full_path);
                    }
                }
            );
        }
        catch (IOError e) {
           critical(e.message);
        }
    }

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

    ~StatusNotifierWatcher() {
        bus.signal_unsubscribe(noc_signal_id);
    }
}
}
