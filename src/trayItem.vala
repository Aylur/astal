using DbusmenuGtk;

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
    public abstract Category Category { owned get; }
    public abstract Status Status { owned get; }
    public abstract Tooltip? ToolTip { owned get; }
    public abstract string Id { owned get; }
    public abstract string? IconThemePath { owned get; }
    public abstract bool ItemIsMenu { owned get; }
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

    public string title { owned get { return proxy.Title; } }
    public Category category { get { return proxy.Category; } }
    public Status status {  get { return proxy.Status; } }
    public Tooltip? tooltip { owned get { return proxy.ToolTip; } }

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

    public string id { owned get { return proxy.Id ;} }
    public string icon_theme_path { owned get { return proxy.IconThemePath ;} }
    public bool is_menu { get { return proxy.ItemIsMenu ;} }

    public string icon_name {
        owned get {
            return proxy.Status == Status.NEEDS_ATTENTION
                ? proxy.AttentionIconName
                : proxy.IconName;
        }
    }

    public Gdk.Pixbuf icon_pixbuf { owned get { return _get_icon_pixbuf(); } }

    public string item_id { get; private set; }

    public signal void changed();
    public signal void ready();

    public TrayItem(string service, string path) {
        connection_ids = new List<ulong>();
        item_id = service + path;
        setup_proxy.begin(service, path, (_, res) => setup_proxy.end(res));
    }

    private async void setup_proxy(string service, string path) {
        try {
            proxy = yield Bus.get_proxy(
                BusType.SESSION,
                service,
                path);

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

            ready();
        } catch (Error err) {
            critical(err.message);
        }
    }

    private void _notify() {
        string[] props = { "category", "id", "title", "status", "is-menu", "tooltip-markup" };

        foreach (string prop in props)
            notify_property(prop);

        changed();
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

                    _notify();
                } catch(Error e) {
                    //silently ignore
                }
            }
        );
    }

    public void activate(int x, int y) {
        try {
            proxy.Activate(x, y);
        } catch (Error e) {
            if(e.domain != DBusError.quark() || e.code != DBusError.UNKNOWN_METHOD)
                warning(e.message);
        }
    }

    public void secondary_activate(int x, int y) {
        try {
            proxy.SecondaryActivate(x, y);
        } catch (Error e) {
            if(e.domain != DBusError.quark() || e.code != DBusError.UNKNOWN_METHOD)
                warning(e.message);
        }
    }

    public void scroll(int delta, string orientation) {
        try {
            proxy.Scroll(delta, orientation);
        } catch (Error e) {
            if(e.domain != DBusError.quark() || e.code != DBusError.UNKNOWN_METHOD) 
                warning("%s\n", e.message);
        }
    }


    public DbusmenuGtk.Menu? create_menu() {
        if (proxy.Menu == null)
            return null;

        return new DbusmenuGtk.Menu(
            proxy.get_name_owner(),
            proxy.Menu);
    }

    public Gdk.Pixbuf? _get_icon_pixbuf() {
        Pixmap[] pixmaps = proxy.Status == Status.NEEDS_ATTENTION
            ? proxy.AttentionIconPixmap
            : proxy.IconPixmap;


        string icon_name = proxy.Status == Status.NEEDS_ATTENTION
            ? proxy.AttentionIconName
            : proxy.IconName;

        Gdk.Pixbuf pixbuf = null;

        if (icon_name != null && proxy.IconThemePath != null)
            pixbuf = load_from_theme(icon_name, proxy.IconThemePath);

        if (pixbuf == null)
            pixbuf = pixmap_to_pixbuf(pixmaps);

        return pixbuf;
    }

    private Gdk.Pixbuf? load_from_theme(string icon_name, string theme_path) {
        if (theme_path == "" || theme_path == null)
            return null;

        if (icon_name == "" || icon_name == null)
            return null;

        Gtk.IconTheme icon_theme = new Gtk.IconTheme();
        string[] paths = {theme_path};
        icon_theme.set_search_path(paths);

        int size = icon_theme.get_icon_sizes(icon_name)[0];
        Gtk.IconInfo icon_info = icon_theme.lookup_icon(
            icon_name, size, Gtk.IconLookupFlags.FORCE_SIZE);

        if (icon_info != null)
            return icon_info.load_icon();

        return null;
    }

    private Gdk.Pixbuf? pixmap_to_pixbuf(Pixmap[] pixmaps) {
        if (pixmaps == null || pixmaps.length == 0)
            return null;

        Pixmap pixmap = pixmaps[0];
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
