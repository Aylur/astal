[DBus(name = "org.freedesktop.Notifications")]
internal class AstalNotifd.Daemon : Object {
    public static string name = "notifd";
    public static string vendor = "astal";
    public static string version = "0.1";

    private string cache_directory;

    private uint n_id = 1;

    private List<weak Notification> notification_list =
        new List<weak Notification>();

    private HashTable<uint, Notification> notifs =
        new HashTable<uint, Notification>((i) => i, (a, b) => a == b);

    public signal void notified(uint id, bool replaced);

    public signal void resolved(uint id, ClosedReason reason) {
        var n = notifs.get(id);
        if (n != null) {
            n.resolved(reason);
            notifs.remove(id);
            notification_list = notifs.get_values().copy();
            notification_closed(id, reason);
            write_state();
        }
    }

    // called from proxy
    public void emit_resolved(uint id, ClosedReason reason) throws Error {
        resolved(id, reason);
    }

    // called from proxy
    public void emit_action_invoked(uint id, string action) throws Error {
        action_invoked(id, action);
    }

    // used by proxy to sync up with daemon
    public Variant get_notification(uint id) throws Error {
        var n = notifs.get(id);
        if (n == null) {
            throw new DBusError.INVALID_ARGS("notification does no exist");
        }
        return n.serialize();
    }

    internal Notification? get_notif(uint id) {
        return notifs.get(id);
    }

    internal List<weak Notification> notifications {
        get { return notification_list; }
    }

    internal void add_notification(Notification n) {
        n.invoked.connect((action) => {
            action_invoked(n.id, action);
            if (!n.resident) resolved(n.id, ClosedReason.CLOSED);
        });
        n.dismissed.connect(() => resolved(n.id, ClosedReason.DISMISSED_BY_USER));
        n.state = State.RECEIVED;
        notifs.set(n.id, n);
        notification_list = notifs.get_values().copy();

        var ignore_timeout = Notifd.settings.get_boolean("ignore-timeout");

        if (!ignore_timeout && (n.expire_timeout > 0)) {
            Timeout.add(n.expire_timeout, () => {
                if (!ignore_timeout) {
                    resolved(n.id, ClosedReason.EXPIRED);
                }
                return Source.REMOVE;
            }, Priority.DEFAULT);
        }
    }

    // spec
    [DBus(name = "Notify")]
    public async uint Notify(
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
            var file = yield cache_image(hints.get("image-data"), app_name);
            if (file != null) {
                hints.set("image-path", new Variant.string(file));
                hints.remove("image-data");
            }
        }

        // deprecated hints, removing them to avoid littering cache with binary data
        hints.remove("image_data");
        hints.remove("icon_data");

        var replaced = notifs.get(replaces_id) != null;
        var id = replaced ? replaces_id : n_id++;

        var n = new Notification() {
            id = id,
            app_name = app_name,
            app_icon = app_icon,
            summary = summary,
            body = body,
            expire_timeout = expire_timeout,
            hints = hints,
            time = new DateTime.now_local().to_unix(),
        };
        foreach (var action in Action.new_list(actions)) {
            n.add_action(action);
        }

        add_notification(n);
        notified(id, replaced);
        write_state();
        return id;
    }

    public string[] get_capabilities() throws DBusError, IOError {
        return Notifd.settings.get_strv("server-capabilites");
    }

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

    public signal void notification_closed(uint id, uint reason);
    public signal void action_invoked(uint id, string action);

    // TODO: expose though Notifd and let users emit it
    public signal void activation_token(uint id, string token);

    // internals

    private async string? cache_image(Variant image, string app_name) {
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

        if (pixbuf == null) return null;

        var file_name = cache_directory + "/" + data.hash().to_string("%u.png");

        try {
            if (!FileUtils.test(cache_directory, FileTest.EXISTS)) {
                File.new_for_path(cache_directory).make_directory_with_parents(null);
            }

            var output_stream = yield File.new_for_path(file_name).replace_async(
                null,
                false,
                FileCreateFlags.NONE,
                Priority.DEFAULT
            );

            yield pixbuf.save_to_streamv_async(output_stream, "png", null, null, null);
            output_stream.close(null);
        } catch (Error err) {
            warning("could not cache image %s", err.message);
            return null;
        }

        return file_name;
    }

    private uint? state_debounce = null;
    private void write_state() {
        if (state_debounce != null) {
            Source.remove(state_debounce);
            state_debounce = null;
        }

        // schedule new one
        state_debounce = GLib.Timeout.add(100, () => {
            state_debounce = null;

            var av = new VariantType.array(new VariantType("v"));
            var builder = new VariantBuilder(av);

            foreach (var n in notifs.get_values()) {
                if (!n.transient) builder.add("v", n.serialize());
            }

            Notifd.settings.set_value("notifications", builder.end());

            return GLib.Source.REMOVE;
        });
    }

    public Daemon(DBusConnection conn) {
        cache_directory = Environment.get_user_cache_dir() + "/astal/notifd";

        var iter = new VariantIter(Notifd.settings.get_value("notifications"));
        Variant? variant;
        while (iter.next("v", out variant)) {
            var n = new Notification.deserialize(variant);
            add_notification(n);
            if (n.id >= n_id) n_id = n.id + 1;
        }

        try {
            conn.register_object("/org/freedesktop/Notifications", this);
        } catch (Error err) {
            critical(err.message);
        }
    }
}
