namespace AstalTray {
[DBus (name="org.kde.StatusNotifierWatcher")]
internal interface IWatcher : Object {
    public abstract string[] RegisteredStatusNotifierItems { owned get; }
    public abstract int ProtocolVersion { get; }

    public abstract void RegisterStatusNotifierItem(string service, BusName sender) throws DBusError, IOError;
    public abstract void RegisterStatusNotifierHost(string service) throws DBusError, IOError;

    public signal void StatusNotifierItemRegistered(string service);
    public signal void StatusNotifierItemUnregistered(string service);
    public signal void StatusNotifierHostRegistered();
    public signal void StatusNotifierHostUnregistered();
}
/**
 * Get the singleton instance of [class@AstalTray.Tray]
 */
public Tray get_default() {
    return Tray.get_default();
}

public class Tray : Object {
    private static Tray? instance;

    /**
     * Get the singleton instance of [class@AstalTray.Tray]
     */
    public static unowned Tray get_default() {
        if (instance == null)
            instance = new Tray();

        return instance;
    }

    private StatusNotifierWatcher watcher;
    private IWatcher proxy;

    private HashTable<string, TrayItem> _items =
        new HashTable<string, TrayItem>(str_hash, str_equal);

    /**
     * List of currently registered tray items
     */
    public List<weak TrayItem> items { owned get { return _items.get_values(); }}

    /**
     * emitted when a new tray item was added.
     */
    public signal void item_added(string item_id) {
        notify_property("items");
    }

    /**
     * emitted when a tray item was removed.
     */
    public signal void item_removed(string item_id) {
        notify_property("items");
    }

    construct {
        try {
            Bus.own_name(
                BusType.SESSION,
                "org.kde.StatusNotifierWatcher",
                BusNameOwnerFlags.NONE,
                start_watcher,
                () => {
                    if (proxy != null) {
                        proxy = null;
                    }
                },
                start_host
            );
        } catch (Error err) {
            critical(err.message);
        }

    }

    private void start_watcher(DBusConnection conn) {
        try {
            watcher = new StatusNotifierWatcher();
            conn.register_object("/StatusNotifierWatcher", watcher);
            watcher.StatusNotifierItemRegistered.connect(on_item_register);
            watcher.StatusNotifierItemUnregistered.connect(on_item_unregister);
        } catch (Error err) {
            critical(err.message);
        }
    }

    private void start_host() {
        if (proxy != null)
            return;

        try {
            proxy = Bus.get_proxy_sync(BusType.SESSION,
                "org.kde.StatusNotifierWatcher",
                "/StatusNotifierWatcher"); 

            proxy.StatusNotifierItemRegistered.connect(on_item_register);
            proxy.StatusNotifierItemUnregistered.connect(on_item_unregister);

            proxy.notify["g-name-owner"].connect(() => {
                _items.foreach((service, _) => {
                    item_removed(service);
                });

                _items.remove_all();

                if(proxy != null) {
                    foreach (string item in proxy.RegisteredStatusNotifierItems) {
                        on_item_register(item);
                    }
                } else {
                    foreach (string item in watcher.RegisteredStatusNotifierItems) {
                        on_item_register(item);
                    }
                }
            });

            foreach (string item in proxy.RegisteredStatusNotifierItems) {
                on_item_register(item);
            }
        } catch (Error err) {
            critical("cannot get proxy: %s", err.message);
        }
    }

    private void on_item_register(string service) {
        if (_items.contains(service))
            return;

        var parts = service.split("/", 2);
        TrayItem item = new TrayItem(parts[0], "/" + parts[1]);
        item.ready.connect(() => {
            _items.set(service, item);
            item_added(service);
        });
    }

    private void on_item_unregister(string service) {
        _items.remove(service);
        item_removed(service);
    }

    /**
     * gets the TrayItem with the given item-id.
     */
    public TrayItem get_item(string item_id) {
        return _items.get(item_id);
    }
}
}
