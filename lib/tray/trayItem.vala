namespace AstalTray {
public struct Pixmap {
    int width;
    int height;
    uint8[] bytes;
}

public struct Tooltip {
    string icon_name;
    Pixmap[] icon;
    string title;
    string description;
}

[DBus (use_string_marshalling = true)]
public enum Category {
    [DBus (value = "ApplicationStatus"), Description (nick = "ApplicationStatus")]
    APPLICATION,

    [DBus (value = "Communications"), Description (nick = "Communications")]
    COMMUNICATIONS,

    [DBus (value = "SystemServices"), Description (nick = "SystemServices")]
    SYSTEM,

    [DBus (value = "Hardware"), Description (nick = "Hardware")]
    HARDWARE;

    public string to_nick () {
        var enumc = (EnumClass)typeof (Category).class_ref();
        unowned var eval = enumc.get_value(this);
        return eval.value_nick;
    }
}


[DBus (use_string_marshalling = true)]
public enum Status {
    [DBus (value = "Passive"), Description (nick = "Passive")]
    PASSIVE,

    [DBus (value = "Active"), Description (nick = "Active")]
    ACTIVE,

    [DBus (value = "NeedsAttention"), Description (nick = "NeedsAttention")]
    NEEDS_ATTENTION;

    public string to_nick () {
        var enumc = (EnumClass)typeof (Status).class_ref();
        unowned var eval = enumc.get_value(this);
        return eval.value_nick;
    }
}

[DBus (name="org.kde.StatusNotifierItem")]
internal interface IItem : DBusProxy {
    public abstract string Title { owned get; }
    public abstract Category Category { get; }
    public abstract Status Status { get; }
    public abstract Tooltip? ToolTip { owned get; }
    public abstract string Id { owned get; }
    public abstract string? IconThemePath { owned get; }
    public abstract bool ItemIsMenu { get; }
    public abstract ObjectPath? Menu { owned get; }
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

    /** The Title of the TrayItem */
    public string title { owned get { return proxy.Title; } }

    /** The category this item belongs to */
    public Category category { get { return proxy.Category; } }

    /** The current status of this item */
    public Status status {  get { return proxy.Status; } }

    /** The tooltip of this item */
    public Tooltip? tooltip { owned get { return proxy.ToolTip; } }

    /**
    * A markup representation of the tooltip. This is basically equvivalent
    * to `tooltip.title \n tooltip.description`
    */
    public string tooltip_markup {
        owned get {
            if (proxy.ToolTip == null)
                return "";

            var tt = proxy.ToolTip.title;
            if (proxy.ToolTip.description != "")
                tt += "\n" + proxy.ToolTip.description;

            return tt;
        }
    }

    /** the id of the item. This id is specified by the tray app.*/
    public string id { owned get { return proxy.Id ;} }

    /**
    * If set, this only supports the menu, so showing the menu should be prefered
    * over calling [method@AstalTray.TrayItem.activate].
    */
    public bool is_menu {
        get {
            return proxy.ItemIsMenu || proxy.get_cached_property("ItemIsMenu") == null;
        }
    }

    /**
    * The icon theme path, where to look for the [property@AstalTray.TrayItem:icon-name].
    * It is recommended to use the [property@AstalTray.TrayItem:gicon] property,
    * which does the icon lookups for you.
    */
    public string icon_theme_path { owned get { return proxy.IconThemePath ;} }

    /**
    * The name of the icon. This should be looked up in the [property@AstalTray.TrayItem:icon-theme-path]
    * if set or in the currently used icon theme otherwise.
    * It is recommended to use the [property@AstalTray.TrayItem:gicon] property,
    * which does the icon lookups for you.
    */
    public string icon_name {
        owned get {
            return proxy.Status == Status.NEEDS_ATTENTION
                ? proxy.AttentionIconName
                : proxy.IconName;
        }
    }

    /**
    * A pixbuf containing the icon.
    * It is recommended to use the [property@AstalTray.TrayItem:gicon] property,
    * which does the icon lookups for you.
    */
    public Gdk.Pixbuf icon_pixbuf { owned get { return _get_icon_pixbuf(); } }

    /**
    * Contains the items icon. This property is intended to be used with the gicon property
    * of the Icon widget and the recommended way to display the icon.
    * This property unifies the [property@AstalTray.TrayItem:icon-name],
    * [property@AstalTray.TrayItem:icon-theme-path] and [property@AstalTray.TrayItem:icon-pixbuf] properties.
    */
    public GLib.Icon gicon { get; private set; }

    /** The id of the item used to uniquely identify the TrayItems by this lib.*/
    public string item_id { get; private set; }

    private DBusMenu.Importer menu_importer;

    /**
    * The MenuModel describing the menu for this TrayItem to be used with a MenuButton or PopoverMenu.
    * The actions for this menu are defined in [property@AstalTray.TrayItem:action-group].
    */
    public MenuModel? menu_model {
        owned get {
            if (menu_importer == null) return null;
            return menu_importer.model;
        }
    }
    
    /**
    * The ActionGroup containing the actions for the menu. All actions have the `dbusmenu` prefix and are
    * setup to work with the [property@AstalTray.TrayItem:menu-model]. Make sure to insert this action group
    * into a parent widget of the menu, eg the MenuButton for which the MenuModel for this TrayItem is set.
    */
    public ActionGroup? action_group {
        owned get {
            if (menu_importer == null) return null;
            return menu_importer.action_group;
        }
    }

    public signal void changed();
    public signal void ready();

    internal TrayItem(string service, string path) {
        connection_ids = new List<ulong>();
        item_id = service + path;
        setup_proxy.begin(service, path, (_, res) => setup_proxy.end(res));
    }

    private async void setup_proxy(string service, string path) {
        try {
            proxy = yield Bus.get_proxy(
                BusType.SESSION,
                service,
                path
            );

            proxy.g_default_timeout = 1000;

            connection_ids.append(proxy.NewStatus.connect(refresh_all_properties));
            connection_ids.append(proxy.NewToolTip.connect(refresh_all_properties));
            connection_ids.append(proxy.NewTitle.connect(refresh_all_properties));
            connection_ids.append(proxy.NewIcon.connect(refresh_all_properties));

            proxy.notify["g-name-owner"].connect(() => {
                if (proxy.g_name_owner == null) {
                    foreach (var id in connection_ids)
                        SignalHandler.disconnect(proxy, id);
                }
            });

            Variant? menuVariant = proxy.get_cached_property("Menu");
            if (proxy.Menu != null && menuVariant != null && menuVariant.is_of_type(VariantType.OBJECT_PATH)) { 
                this.menu_importer = new DBusMenu.Importer(proxy.get_name_owner(), proxy.Menu);
                this.menu_importer.notify["model"].connect(() => {
                    notify_property("menu-model");
                    notify_property("action-group");
                });
            }

            update_gicon();
            ready();
        } catch (Error err) {
            critical(err.message);
        }
    }

    private void _notify() {
        string[] props = {
            "category",
            "id",
            "title",
            "status",
            "is-menu",
            "tooltip-markup",
            "icon-name",
            "icon-pixbuf"
        };

        foreach (string prop in props) {
            notify_property(prop);
        }

        changed();
    }

    private void update_gicon() {
        if (icon_name != null && icon_name != "") {
            if (GLib.FileUtils.test(icon_name, GLib.FileTest.EXISTS)) {
                gicon = new GLib.FileIcon(GLib.File.new_for_path(icon_name));
            }
            else if(icon_theme_path != null && icon_theme_path != "") {
                gicon = new GLib.FileIcon(GLib.File.new_for_path(
                    find_icon_in_theme(icon_name, icon_theme_path)
                ));
            } else {
                gicon = new GLib.ThemedIcon(icon_name);
            }
        }
        else {
            Pixmap[] pixmaps = proxy.Status == Status.NEEDS_ATTENTION
                ? proxy.AttentionIconPixmap
                : proxy.IconPixmap;
            gicon = pixmap_to_pixbuf(pixmaps);
        }
    }


    private void refresh_all_properties() {
        proxy.g_connection.call.begin(
            proxy.g_name,
            proxy.g_object_path,
            "org.freedesktop.DBus.Properties",
            "GetAll",
            new Variant("(s)", proxy.g_interface_name),
            new VariantType("(a{sv})"),
            DBusCallFlags.NONE,
            -1,
            null,
            (_, result) => {
                try {
                    Variant parameters = proxy.g_connection.call.end(result);
                    VariantIter prop_iter;
                    parameters.get("(a{sv})", out prop_iter);

                    string prop_key;
                    Variant prop_value;

                    while (prop_iter.next ("{sv}", out prop_key, out prop_value)) {
                        proxy.set_cached_property(prop_key, prop_value);
                    }

                    update_gicon();

                    _notify();
                } catch(Error e) {
                    //silently ignore
                }
            }
        );
    }

    /**
    * tells the tray app that its menu is about to be opened, 
    * so it can update the menu if needed. You should call this method
    * before openening the menu.
    */
    public void about_to_show() {
      if(proxy.Menu == null) return;
      try {
        Bus.get_sync(BusType.SESSION).call_sync(
          this.proxy.g_name_owner,
          this.proxy.Menu,
          "com.canonical.dbusmenu",
          "AboutToShow",
          new Variant("(i)", 0),
          null,
          DBusCallFlags.NONE,
          -1,
          null
        );
      }
      catch (Error r) {
        //silently ignore
      }
    }

    /**
    * Send an activate request to the tray app.
    */
    public void activate(int x, int y) {
        try {
            proxy.Activate(x, y);
        } catch (Error e) {
            if (e.domain != DBusError.quark() || e.code != DBusError.UNKNOWN_METHOD) {
                warning(e.message);
            }
        }
    }

    /**
    * Send a secondary activate request to the tray app.
    */
    public void secondary_activate(int x, int y) {
        try {
            proxy.SecondaryActivate(x, y);
        } catch (Error e) {
            if (e.domain != DBusError.quark() || e.code != DBusError.UNKNOWN_METHOD) {
                warning(e.message);
            }
        }
    }

    /**
    * Send a scroll request to the tray app.
    * valid values for the orientation are "horizontal" and "vertical".
    */
    public void scroll(int delta, string orientation) {
        try {
            proxy.Scroll(delta, orientation);
        } catch (Error e) {
            if(e.domain != DBusError.quark() || e.code != DBusError.UNKNOWN_METHOD) 
                warning("%s\n", e.message);
        }
    }

    private string? find_icon_in_theme(string icon_name, string theme_path){
        if (icon_name == null || theme_path == null || icon_name == "" || theme_path == "") {
            return null;
        }

        try {
            Dir dir = Dir.open(theme_path, 0);
            string? name = null;

            while ((name = dir.read_name ()) != null) {
                var path = Path.build_filename(theme_path, name);

                if (FileUtils.test(path, FileTest.IS_DIR)) {
                    string? icon = find_icon_in_theme(icon_name, path);
                    if (icon != null) return icon;
                    else continue;
                }

                int dot_index = name.last_index_of(".");
                if (dot_index != -1) {
                    name = name.substring(0, dot_index);
                }

                if (name == icon_name) return path;
            }
        } catch (FileError err) {
            return null;
        }
        return null;

    }

    private Gdk.Pixbuf? _get_icon_pixbuf() {
        Pixmap[] pixmaps = proxy.Status == Status.NEEDS_ATTENTION
            ? proxy.AttentionIconPixmap
            : proxy.IconPixmap;

        return pixmap_to_pixbuf(pixmaps);
    }

    private Gdk.Pixbuf? pixmap_to_pixbuf(Pixmap[] pixmaps) {
        if (pixmaps == null || pixmaps.length == 0)
            return null;

        Pixmap pixmap = pixmaps[0];

        for (int i = 0; i < pixmaps.length; i++){
            if(pixmap.width < pixmaps[i].width)
              pixmap = pixmaps[i];
        };

        uint8[] image_data = pixmap.bytes.copy();

        for (int i = 0; i < pixmap.width * pixmap.height * 4; i += 4) {
            uint8 alpha = image_data[i];
            image_data[i] = image_data[i + 1];
            image_data[i + 1] = image_data[i + 2];
            image_data[i + 2] = image_data[i + 3];
            image_data[i + 3] = alpha;
        }

        return new Gdk.Pixbuf.from_bytes(
            new Bytes(image_data),
            Gdk.Colorspace.RGB,
            true,
            8,
            (int)pixmap.width,
            (int)pixmap.height,
            (int)(pixmap.width * 4)
        );
    }

    public string to_json_string() {
    	var generator = new Json.Generator();
        generator.set_root(to_json());
        return generator.to_data(null);
    }

    internal Json.Node to_json() {
        return new Json.Builder()
            .begin_object()
            .set_member_name("item_id").add_string_value(item_id)
            .set_member_name("id").add_string_value(id)
            .set_member_name("bus_name").add_string_value(proxy.g_name)
            .set_member_name("object_path").add_string_value(proxy.g_object_path)
            .set_member_name("title").add_string_value(title)
            .set_member_name("status").add_string_value(status.to_nick())
            .set_member_name("category").add_string_value(category.to_nick())
            .set_member_name("tooltip").add_string_value(tooltip_markup)
            .set_member_name("icon_theme_path").add_string_value(proxy.IconThemePath)
            .set_member_name("icon_name").add_string_value(icon_name)
            .set_member_name("menu_path").add_string_value(proxy.Menu)
            .set_member_name("is_menu").add_boolean_value(is_menu)
            .end_object()
            .get_root();
    }
}
}
