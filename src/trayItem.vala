namespace AstalTray {

  public struct Pixmap {
    int width;
    int height;
    uint8[] bytes;
  }
  
  public struct Tooltip {
    string icon_name;
    Pixmap icon;
    string title;
    string description;
  }

  [DBus (use_string_marshalling = true)]
  public enum Category {
    [DBus (value = "ApplicationStatus")]
    APPLICATION,
    [DBus (value = "Communications")]
    COMMUNICATIONS,
    [DBus (value = "SystemServices")]
    SYSTEM,
    [DBus (value = "Hardware")]
    HARDWARE
  }

  [DBus (use_string_marshalling = true)]
  public enum Status {
    [DBus (value = "Passive")]
    PASSIVE,
    [DBus (value = "Active")]
    ACTIVE,
    [DBus (value = "NeedsAttention")]
    NEEDS_ATTENTION
  }

[DBus (name="org.kde.StatusNotifierItem")]
internal interface IItem : DBusProxy {

  public abstract string Title { owned get; }
  public abstract Category Category { owned get; }
  public abstract Status Status { owned get; }
  public abstract Tooltip Tooltip { owned get; }
  public abstract string Id { owned get; }
  public abstract string IconThemePath { owned get; }
  public abstract bool ItemIsMenu { owned get; }
  public abstract ObjectPath Menu { owned get; }
  public abstract string IconName { owned get; }
  public abstract Pixmap[] IconPixmap { owned get; }
  public abstract string AttentionIconName { owned get; }
  public abstract Pixmap[] AttentionIconPixmap { owned get; }
  public abstract string OverlayIconName { owned get; }
  public abstract Pixmap[] OverlayIconPixmap { owned get; }
  
  public abstract void ContexMenu(int x, int y) throws DBusError, IOError;
  public abstract void Activate(int x, int y) throws DBusError, IOError;
  public abstract void SecondaryActivate(int x, int y) throws DBusError, IOError;
  public abstract void Scroll(int delta, string orientation) throws DBusError, IOError;
  
  public signal void NewTitle();
  public signal void NewIcon();
  public signal void NewAttentionIcon();
  public signal void NewOverlayIcon();
  public signal void NewToolTip();
  public signal void NewStatus(string status);

}

public class TrayItem : Object {

    private IItem proxy;
    private List<ulong> connection_ids;
    
    public string title { owned get { return proxy.Title; } }
    public Category category { get { return proxy.Category; } }
    public Status status {  get { return proxy.Status; } }
    public Tooltip tooltip { owned get { return proxy.Tooltip; } }
    public string tooltip_string { owned get { return proxy.Tooltip.title; } }
    public string id { owned get { return proxy.Id ;} }
    public string icon_theme_path { owned get { return proxy.IconThemePath ;} }
    public bool is_menu { get { return proxy.ItemIsMenu ;} }

    public signal void removed();

    public TrayItem(string service, string path) {
      
      connection_ids = new List<ulong>();

      proxy = Bus.get_proxy_sync(
        BusType.SESSION,
        service,
        path
      );
      
      //connection_ids.append(proxy.NewIcon.connect(() => notify(icon)));
      connection_ids.append(proxy.NewTitle.connect(() => notify_property("title")));
      connection_ids.append(proxy.NewToolTip.connect(() => {
        notify_property("tooltip");
        notify_property("tooltip_string");
      }));
      connection_ids.append(proxy.NewStatus.connect(() => notify_property("status")));

      proxy.notify["g-name-owner"].connect(
        () => {
          if (proxy.g_name_owner == null) {
            foreach (var id in connection_ids)
              SignalHandler.disconnect(proxy, id);

            removed();
          }
        }
      );
    }
}
}


