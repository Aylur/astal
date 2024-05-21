namespace AstalNotifd {
public enum ClosedReason {
    EXPIRED = 1,
    DISMISSED_BY_USER = 2,
    CLOSED = 3,
    UNDEFINED = 4,
}

[DBus (name = "org.freedesktop.Notifications")]
internal class Daemon : Object {
    public static string name = "notifd";
    public static string vendor = "astal";
    public static string version = "0.1";

    private string cache_file;
    private uint n_id = 1;
    private HashTable<uint, Notification> notifs =
        new HashTable<uint, Notification>((i) => i, (a, b) => a == b);

    public string cache_directory { set; owned get; }
    public bool ignore_timeout { get; set; }
    public bool dont_disturb { get; set; }

    public signal void notified(uint id);
    public signal void resolved(uint id, ClosedReason reason);

    construct {
        if (cache_directory == null)
            cache_directory = Environment.get_user_cache_dir() + "/astal/notifd";

        cache_file = cache_directory + "/notifications.json";

        if (FileUtils.test(cache_file, FileTest.EXISTS)) {
            try {
                uint8[] json;
                File.new_for_path(cache_file).load_contents(null, out json, null);
                var parser = new Json.Parser();
                parser.load_from_data((string)json);
                var list = parser.get_root().get_array();
                for (var i = 0; i < list.get_length(); ++i) {
                    var n = new Notification.from_json(list.get_object_element(i));
                    notifs.set(n.id, n);
                }
            } catch (Error err) {
                warning("failed to load cache: %s", err.message);
            }
        }

        notified.connect(() => {
            notify_property("notifications");
        });

        resolved.connect((id, reason) => {
            notifs.get(id).resolved(reason);
            notifs.remove(id);
            cache();
            notify_property("notifications");
            notification_closed(id, reason);
        });
    }

    public uint[] notification_ids() throws DBusError, IOError {
        var keys = notifs.get_keys();
        uint[] id = new uint[keys.length()];
        for (var i = 0; i < keys.length(); ++i)
            id[i] = keys.nth_data(i);
        return id;
    }

    [DBus (visible = false)]
    public List<weak Notification> notifications {
        owned get { return notifs.get_values(); }
    }

    [DBus (visible = false)]
    public Notification get_notification(uint id) throws DBusError, IOError {
        return notifs.get(id);
    }

    public string get_notification_json(uint id) throws DBusError, IOError {
        return notifs.get(id).to_json_string();
    }

    [DBus (name = "Notify")]
    public uint Notify(
        string app_name,
        uint replaces_id,
        string app_icon,
        string summary,
        string body,
        string[] actions,
        HashTable<string, Variant> hints,
        int expire_timeout
    ) throws DBusError, IOError {
        if (hints.get("image-data") != null) {
            var file = cache_image(hints.get("image-data"), app_name);
            if (file != null) {
                hints.set("image-path", new Variant.string(file));
                hints.remove("image-data");
            }
        }

        // deprecated hints
        hints.remove("image_data");
        hints.remove("icon_data");

        var id = replaces_id > 0 ? replaces_id : n_id++;
        var n = new Notification(
            app_name, id, app_icon, summary, body, actions, hints, expire_timeout
        );

        n.dismissed.connect(() => resolved(id, ClosedReason.DISMISSED_BY_USER));
        n.invoked.connect((action) => action_invoked(id, action));

        if (!ignore_timeout && expire_timeout > 0) {
            Timeout.add(expire_timeout, () => {
                resolved(id, ClosedReason.EXPIRED);
                return Source.REMOVE;
            }, Priority.DEFAULT);
        }

        notifs.set(id, n);
        if (!dont_disturb)
            notified(id);

        cache();
        return id;
    }

    private void cache() {
        var list = new Json.Builder().begin_array();
        foreach (var n in notifications) {
            list.add_value(n.to_json());
        }
        list.end_array();
    	var generator = new Json.Generator();
        generator.set_root(list.get_root());
        var json = generator.to_data(null);

        try {
            if (!FileUtils.test(cache_directory, FileTest.EXISTS))
                File.new_for_path(cache_directory).make_directory_with_parents(null);

            FileUtils.set_contents_full(cache_file, json);
        } catch (Error err) {
            warning("failed to cache notifications: %s", err.message);
        }
    }

    public signal void notification_closed(uint id, uint reason);
    public signal void action_invoked(uint id, string action);
    public signal void activation_token(uint id, string token);

    public void close_notification(uint id) throws DBusError, IOError {
        resolved(id, ClosedReason.CLOSED);
    }

    public void get_server_information(
        out string name,
        out string vendor,
        out string version,
        out string spec_version
    ) throws DBusError, IOError {
        name = Daemon.name;
        vendor = Daemon.vendor;
        version = Daemon.version;
        spec_version = "1.2";
    }

    public string[] get_capabilities() throws DBusError, IOError {
        return {"action-icons", "actions", "body", "icon-static", "persistence", "sound"};
    }

    private string? cache_image(Variant image, string app_name) {
        int w = image.get_child_value(0).get_int32();
        int h = image.get_child_value(1).get_int32();
        int rs = image.get_child_value(2).get_int32();
        bool alpha = image.get_child_value(3).get_boolean();
        int bps = image.get_child_value(4).get_int32();
        Bytes data = image.get_child_value(6).get_data_as_bytes();

        if (bps != 8) {
            warning("Can not cache image from %s. %s", app_name,
                "Currently only RGB images with 8 bits per sample are supported.");
            return null;
        }

        var pixbuf = new Gdk.Pixbuf.from_bytes(
            data, Gdk.Colorspace.RGB, alpha, bps, w, h, rs);

        if (pixbuf == null)
            return null;

        var file_name = cache_directory + "/" + data.hash().to_string("%u.png");

        try {
            var output_stream = File.new_for_path(file_name)
                .replace(null, false, FileCreateFlags.NONE, null);

            pixbuf.save_to_streamv(output_stream, "png", null, null, null);
            output_stream.close(null);
        } catch (Error err) {
            warning("could not cache image %s", err.message);
            return null;
        }

        return file_name;
    }

    [DBus (visible = false)]
    public Daemon register(DBusConnection conn) {
        try {
            conn.register_object("/org/freedesktop/Notifications", this);
        } catch (Error err) {
            critical(err.message);
        }
        return this;
    }
}
}
