namespace AstalTray {

[DBus (name="org.kde.StatusNotifierWatcher")]
internal interface IWatcher : Object {

  public abstract string[] RegisteredStatusNotifierItems { owned get; }
  public abstract int ProtocolVersion { owned get; }

  public abstract void RegisterStatusNotifierItem(string service, BusName sender) throws DBusError, IOError;
  public abstract void RegisterStatusNotifierHost(string service) throws DBusError, IOError;
  
  public signal void StatusNotifierItemRegistered(string service);
  public signal void StatusNotifierItemUnregistered(string service);
  public signal void StatusNotifierHostRegistered();
  public signal void StatusNotifierHostUnregistered();

}

public class Tray : Object {

    private StatusNotifierWatcher watcher;
    private IWatcher proxy;

    private HashTable<string, TrayItem> _items;
    public TrayItem[] items { get { return _items.get_values_as_ptr_array().data; }}
    
    public signal void item_added(string service);
    public signal void item_removed(string service);

    construct {
      _items = new HashTable<string, TrayItem>(GLib.str_hash, GLib.str_equal);
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
          critical("%s", err.message);
      }

    }

    private void start_watcher(DBusConnection conn) {
      watcher = new StatusNotifierWatcher();
      conn.register_object("/StatusNotifierWatcher", watcher);
      watcher.StatusNotifierItemRegistered.connect(on_item_register);
      watcher.StatusNotifierItemUnregistered.connect(on_item_unregister);
    }

    private void start_host() {
      if(proxy != null) return;
      
      proxy = Bus.get_proxy_sync(
          BusType.SESSION,
          "org.kde.StatusNotifierWatcher",
          "/StatusNotifierWatcher"
        ); 
      
      proxy.StatusNotifierItemRegistered.connect(on_item_register);
      proxy.StatusNotifierItemUnregistered.connect(on_item_unregister);
    }

    private void on_item_register(string service) {
      if(_items.contains(service)) return;
      string[] parts = service.split("/", 2);
      TrayItem item = new TrayItem(parts[0], "/" + parts[1]);
      _items.set(service, item);
      item_added(service);
    }

    private void on_item_unregister(string service) {
      string[] parts = service.split("/", 2);
      _items.remove(service);
      item_removed(service);
    }

    public TrayItem get_item(string service) {
      return _items.get(service);
    }

}
}


