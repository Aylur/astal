namespace AstalTray {
public struct Pixmap {
    int width;
    int height;
    uint8[] bytes;

    internal static Pixmap from_variant(GLib.Variant variant) {
        Pixmap pixmap = new Pixmap();

        int width, height;
        Variant data;

        variant.get("(ii@ay)", out width, out height, out data);
        pixmap.width = width;
        pixmap.height = height;
        pixmap.bytes = data.get_data_as_bytes().get_data();


        return pixmap;
    }

    internal static Pixmap[] array_from_variant(GLib.Variant variant) {
        Pixmap[] icons = new Pixmap[0];

        GLib.VariantIter iter = variant.iterator();

        GLib.Variant child;
        while ((child = iter.next_value()) != null) {
            Pixmap pm = Pixmap.from_variant(child);
            icons += pm;
        }
        return icons;
    }
}

public struct Tooltip {
    string icon_name;
    Pixmap[] icon;
    string title;
    string description;

    
    internal static Tooltip from_variant(GLib.Variant variant) {
        Tooltip tooltip = Tooltip();

        string icon_name;
        GLib.VariantIter iter;
        string title;
        string description;

        variant.get("(sa(iiay)ss)", out icon_name, out iter, out title, out description);
        tooltip.icon_name = icon_name;
        tooltip.title = title;
        tooltip.description = description;

        int x, y;
        uint8[] data;
        GLib.Variant child;
       
        Pixmap[] icons = new Pixmap[0];

        while ((child = iter.next_value()) != null) {
            Pixmap pm = Pixmap.from_variant(child);
            icons += pm;
        }

        tooltip.icon = icons;

        return tooltip;
    }
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

    public static extern Category from_string(string value) throws Error;
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

    public static extern Status from_string(string value) throws Error;
}

[DBus (name="org.kde.StatusNotifierItem")]
internal interface IItem : DBusProxy {
    
    // public abstract string Id { owned get; }
    // public abstract Category Category { get; }
    // public abstract string Title { owned get; }
    // public abstract Status Status { get; }
    // public abstract Tooltip? ToolTip { owned get; }
    // public abstract string IconThemePath { owned get; }
    // public abstract bool ItemIsMenu { get; }
    // public abstract ObjectPath? Menu { owned get; }
    // public abstract string IconName { owned get; }
    // public abstract Pixmap[] IconPixmap { owned get; }
    // public abstract string AttentionIconName { owned get; }
    // public abstract Pixmap[] AttentionIconPixmap { owned get; }
    // public abstract string OverlayIconName { owned get; }
    // public abstract Pixmap[] OverlayIconPixmap { owned get; }

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
    private bool needs_update = false;


    /** The Title of the TrayItem */
    public string title { get; private set; }

    /** The category this item belongs to */
    public Category category { get; private set; }

    /** The current status of this item */
    public Status status {  get; private set; }

    /** The tooltip of this item */
    public Tooltip? tooltip { get; private set; }

    /**
    * A markup representation of the tooltip. This is basically equvivalent
    * to `tooltip.title \n tooltip.description`
    */
    public string tooltip_markup {
        owned get {
            if (tooltip == null)
                return "";

            var tt = GLib.Markup.escape_text(tooltip.title);
            if (tooltip.description != "")
                tt += "\n" + tooltip.description;

            return tt;
        }
    }

    /**
    * A text representation of the tooltip. This is basically equvivalent
    * to `tooltip.title \n tooltip.description.`
    */
    public string tooltip_text {
        owned get {
            if (tooltip == null)
                return "";

            var tt = tooltip.title;
            if (tooltip.description != "")
                tt += "\n" + tooltip.description;

            return tt;
        }
    }

    /** the id of the item. This id is specified by the tray app.*/
    public string id { get; private set; }

    /**
    * If set, this only supports the menu, so showing the menu should be prefered
    * over calling [method@AstalTray.TrayItem.activate].
    */
    public bool is_menu { get; private set; default = true; }

    /**
    * The icon theme path, where to look for the [property@AstalTray.TrayItem:icon-name].
    * It is recommended to use the [property@AstalTray.TrayItem:gicon] property,
    * which does the icon lookups for you.
    */
    public string icon_theme_path { get; private set; }

    // icon properties from the dbus for internal use only
    private string IconName;
    private Pixmap[] IconPixmap;
    private string AttentionIconName;
    private Pixmap[] AttentionIconPixmap;
    private string OverlayIconName;
    private Pixmap[] OverlayIconPixmap;

    /**
    * The name of the icon. This should be looked up in the [property@AstalTray.TrayItem:icon-theme-path]
    * if set or in the currently used icon theme otherwise.
    * It is recommended to use the [property@AstalTray.TrayItem:gicon] property,
    * which does the icon lookups for you.
    */
    public string icon_name {
        owned get {
            return status == Status.NEEDS_ATTENTION
                ? AttentionIconName
                : IconName;
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

    /** The object path to the dbusmenu */
    public ObjectPath menu_path { get; private set; }

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

            proxy.g_signal.connect(handle_signal);

            yield refresh_all_properties();
            ready();
        } catch (Error err) {
            critical(err.message);
        }
    }

    private void update_gicon() {
        if (icon_name != null && icon_name != "") {
            if (GLib.FileUtils.test(icon_name, GLib.FileTest.EXISTS)) {
                gicon = new GLib.FileIcon(GLib.File.new_for_path(icon_name));
            }
            else if(icon_theme_path != null && icon_theme_path != "") {
                string path = find_icon_in_theme(icon_name, icon_theme_path);
                if(path != null) gicon = new GLib.FileIcon(GLib.File.new_for_path(path));
                else gicon = new GLib.ThemedIcon(icon_name);
            } else {
                gicon = new GLib.ThemedIcon(icon_name);
            }
        }
        else {
            Pixmap[] pixmaps = status == Status.NEEDS_ATTENTION
                ? AttentionIconPixmap
                : IconPixmap;
            gicon = pixmap_to_pixbuf(pixmaps);
        }
    }

    private void handle_signal(DBusProxy proxy, string? sender_name, string signal_name, Variant parameters) {
      if (needs_update) return;
      needs_update = true;
      GLib.Timeout.add_once(10, () => {
          needs_update = false;
          refresh_all_properties.begin();
      });
    }


    private void set_dbus_property(string prop_name, Variant prop_value) {
        try {
            switch(prop_name) {
                case "Category": {
                    var new_category = Category.from_string(prop_value.get_string());
                    if (category != new_category) {
                        category = new_category;
                    }
                    break;
                }
                case "Id": {
                    var new_id = prop_value.get_string();
                    if (id != new_id) {
                        id = new_id;
                    }
                    break;
                }
                case "Title": {
                    var new_title = prop_value.get_string();
                    if (title != new_title) {
                        title = new_title;
                    }
                    break;
                }
                case "Status": {
                    var new_status = Status.from_string(prop_value.get_string());
                    if (status != new_status) {
                        status = new_status;
                        update_gicon();
                    }
                    break;
                }
                case "ToolTip": {
                    tooltip = Tooltip.from_variant(prop_value);
                    break;
                }
                case "IconThemePath": {
                    var new_path = prop_value.get_string();
                    if (icon_theme_path != new_path) {
                        icon_theme_path = new_path;
                        update_gicon();
                    }
                    break;
                }
                case "ItemIsMenu": {
                    var new_is_menu = prop_value.get_boolean();
                    if (is_menu != new_is_menu) {
                        is_menu = new_is_menu;
                    }
                    break;
                }
                case "Menu": {
                    if(!prop_value.is_of_type(VariantType.OBJECT_PATH)) break;
                    var new_menu_path = (ObjectPath) prop_value.get_string();
                    if (new_menu_path != menu_path) {
                        menu_path = new_menu_path;
                        if (menu_path != null) { 
                            this.menu_importer = new DBusMenu.Importer(proxy.get_name_owner(), menu_path);
                            this.menu_importer.notify["model"].connect(() => {
                                notify_property("menu-model");
                                notify_property("action-group");
                            });
                        } else {
                            this.menu_importer = null;
                            notify_property("menu-model");
                            notify_property("action-group");
                        }
                    }
                    break;
                }
                case "IconName": {
                    var new_icon_name = prop_value.get_string();
                    if (IconName != new_icon_name) {
                        IconName = new_icon_name;
                        notify_property("icon-name");
                        update_gicon();
                    }
                    break;
                }
                case "IconPixmap": {
                    IconPixmap = Pixmap.array_from_variant(prop_value);
                    update_gicon();
                    notify_property("icon-pixbuf");
                    break;
                }
                case "AttentionIconName": {
                    var new_attention_icon_name = prop_value.get_string();
                    if (AttentionIconName != new_attention_icon_name) {
                        AttentionIconName = new_attention_icon_name;
                        update_gicon();
                        notify_property("icon-name");
                    }
                    break;
                }
                case "AttentionIconPixmap": {
                    AttentionIconPixmap = Pixmap.array_from_variant(prop_value);
                    update_gicon();
                    notify_property("icon-pixbuf");
                    break;
                }
                case "OverlayIconName": {
                    var new_overlay_icon_name = prop_value.get_string();
                    if (OverlayIconName != new_overlay_icon_name) {
                        OverlayIconName = new_overlay_icon_name;
                        update_gicon();
                        notify_property("icon-name");
                    }
                    break;
                }
                case "OverlayIconPixmap": {
                    OverlayIconPixmap = Pixmap.array_from_variant(prop_value);
                    update_gicon();
                    notify_property("icon-pixbuf");
                    break;
                }
            }
        }
        catch(Error e) {
            //silently ignore
        }
    }


    private async void refresh_all_properties() {
      this.freeze_notify();
      try {
          Variant parameters = yield proxy.g_connection.call(
              proxy.g_name,
              proxy.g_object_path,
              "org.freedesktop.DBus.Properties",
              "GetAll",
              new Variant("(s)", proxy.g_interface_name),
              new VariantType("(a{sv})"),
              DBusCallFlags.NONE,
              -1,
              null);

          VariantIter prop_iter;
          parameters.get("(a{sv})", out prop_iter);

          string prop_key;
          Variant prop_value;

          while (prop_iter.next ("{sv}", out prop_key, out prop_value)) {
              set_dbus_property(prop_key, prop_value);
          }
      }
      catch(Error e) {
        //silently ignode
      }
      this.thaw_notify();
      this.changed();
    }

    /**
    * tells the tray app that its menu is about to be opened, 
    * so it can update the menu if needed. You should call this method
    * before openening the menu.
    */
    public void about_to_show() {
      if(menu_path == null) return;
      try {
        Bus.get_sync(BusType.SESSION).call_sync(
          this.proxy.g_name_owner,
          menu_path,
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
        Pixmap[] pixmaps = status == Status.NEEDS_ATTENTION
            ? AttentionIconPixmap
            : IconPixmap;

        return pixmap_to_pixbuf(pixmaps);
    }

    private Gdk.Pixbuf? pixmap_to_pixbuf(Pixmap[] pixmaps) {
        if (pixmaps == null || pixmaps.length <= 0)
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
            .set_member_name("icon_theme_path").add_string_value(icon_theme_path)
            .set_member_name("icon_name").add_string_value(icon_name)
            .set_member_name("menu_path").add_string_value(menu_path)
            .set_member_name("is_menu").add_boolean_value(is_menu)
            .end_object()
            .get_root();
    }
}
}
