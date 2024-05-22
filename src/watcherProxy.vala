namespace AstalTray {

[DBus (name="org.kde.StatusNotifierWatcher")]
internal interface IWatcher : Object {

  public abstract string[] RegisteredStatusNotifierItems { get; }
  public abstract int ProtocolVersion { get; }

  public abstract void RegisterStatusNotifierItem(string service, BusName sender) throws DBusError, IOError;
  public abstract void RegisterStatusNotifierHost(string service) throws DBusError, IOError;
  
  public signal void StatusNotifierItemRegistered(string service);
  public signal void StatusNotifierItemUnregistered(string service);
  public signal void StatusNotifierHostRegistered();
  public signal void StatusNotifierHostUnregistered();

}


internal class StatusNotifierWatcherProxy : Object {


    private IWatcher proxy;

    public string[] RegisteredStatusNotifierItems { get { return proxy.RegisteredStatusNotifierItems; } }
    public int ProtocolVersion { get {return proxy.ProtocolVersion;} }

    public signal void StatusNotifierItemRegistered(string service);
    public signal void StatusNotifierItemUnregistered(string service);

    construct {

      proxy = Bus.get_proxy_sync(
          BusType.SESSION,
          "org.kde.StatusNotifierWatcher",
          "/StatusNotifierWatcher"
        ); 
     
      foreach (string item in proxy.RegisteredStatusNotifierItems) {
        StatusNotifierItemRegistered(item);
      }

      proxy.StatusNotifierItemRegistered.connect((s) => StatusNotifierItemRegistered(s));
      proxy.StatusNotifierItemUnregistered.connect((s) => StatusNotifierItemUnregistered(s));

    }

}
}
