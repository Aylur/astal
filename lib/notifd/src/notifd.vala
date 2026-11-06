namespace AstalNotifd {
    /**
     * Get the singleton instance of [class@AstalNotifd.Notifd].
     */
    public Notifd get_default() {
        return Notifd.get_default();
    }
}

/**
 * The notification daemon.
 *
 * This class queues up to become the next daemon while acting as a proxy in the meantime.
 */
public class AstalNotifd.Notifd : Object, ListModel {
    internal static Settings settings;

    private List<weak Notification> notification_list = new List<weak Notification>();

    private static Notifd _instance;
    private Notifd() {}

    /**
     * Get the singleton instance.
     */
    public static Notifd get_default() {
        if (_instance == null) _instance = new Notifd();
        return _instance;
    }

    internal Daemon daemon;
    internal Proxy proxy;

    /**
     * Ignore the timeout specified by incoming notifications.
     * By default notifications can specify a timeout in milliseconds
     * after which the daemon will resolve them even without user input.
     */
    public bool ignore_timeout {
        get { return Notifd.settings.get_boolean("ignore-timeout"); }
        set { Notifd.settings.set_boolean("ignore-timeout", value); }
    }

    /**
     * Tells frontends not to show popups to the user.
     * This property does not have any effect on its own; it is merely
     * a value shared between the daemon process and proxies.
     */
    public bool dont_disturb {
        get { return Notifd.settings.get_boolean("dont-disturb"); }
        set { Notifd.settings.set_boolean("dont-disturb", value); }
    }

    /**
     * Timeout used for notifications that do not specify a timeout and let
     * the server decide. Negative values result in no timeout. By default this is -1.
     */
    public int default_timeout {
        get { return Notifd.settings.get_int("default-timeout"); }
        set { Notifd.settings.set_int("default-timeout", value); }
    }

    /**
     * List of currently unresolved notifications.
     */
    public List<weak Notification> notifications {
        get {
            if (proxy != null) return proxy.notifications;
            if (daemon != null) return daemon.notifications;
            return notification_list;
        }
    }

    /**
     * Gets the [class@AstalNotifd.Notification] with the given ID, or null if there is no such notification.
     */
    public Notification? get_notification(uint id) {
        if (proxy != null) return proxy.get_notif(id);
        if (daemon != null) return daemon.get_notif(id);
        return null;
    }

    /**
     * Emitted when the daemon receives a [class@AstalNotifd.Notification].
     *
     * @param id The ID of the notification.
     * @param replaced Indicates whether an existing notification was replaced.
     */
    public signal void notified(uint id, bool replaced) {
        sync_notifications();
    }

    /**
     * Emitted when a [class@AstalNotifd.Notification] is resolved.
     *
     * @param id The ID of the notification.
     * @param reason The reason the notification was resolved.
     */
    public signal void resolved(uint id, ClosedReason reason) {
        sync_notifications();
    }

    private void sync_notifications() {
        List<weak Notification> next = notifications.copy();

        uint position = 0;
        var previous_length = notification_list.length();
        var next_length = next.length();
        while (
            position < previous_length &&
            position < next_length &&
            notification_list.nth_data(position) == next.nth_data(position)
        ) {
            position++;
        }

        var previous_end = previous_length;
        var next_end = next_length;
        while (
            previous_end > position &&
            next_end > position &&
            notification_list.nth_data(previous_end - 1) == next.nth_data(next_end - 1)
        ) {
            previous_end--;
            next_end--;
        }

        notification_list = (owned)next;

        if ((position < previous_end) || (position < next_end)) {
            items_changed(position, previous_end - position, next_end - position);
        }

        notify_property("notifications");
    }

    public Object? get_item(uint position) {
        if ((proxy != null) && (position < proxy.notifications.length())) {
            return proxy.notifications.nth_data(position);
        }
        if ((daemon != null) && (position < daemon.notifications.length())) {
            return daemon.notifications.nth_data(position);
        }
        return null;
    }

    public Type get_item_type() {
        return typeof(Notification);
    }

    public uint get_n_items() {
        if (proxy != null) return proxy.notifications.length();
        if (daemon != null) return daemon.notifications.length();
        return 0;
    }

    class construct {
        Notifd.settings = new Settings("io.astal.notifd");
    }

    internal signal void active();

    construct {
        Notifd.settings.changed["ignore-timeout"].connect(() => {
            notify_property("ignore-timeout");
        });

        Notifd.settings.changed["dont-disturb"].connect(() => {
            notify_property("dont-disturb");
        });

        Notifd.settings.changed["default-timeout"].connect(() => {
            notify_property("default-timeout");
        });

        // hack to make it synchronous
        MainLoop? loop = null;

        if (!MainContext.default().is_owner()) {
            loop = new MainLoop();
        }

        bool done = false;

        Bus.own_name(
            BusType.SESSION,
            "org.freedesktop.Notifications",
            BusNameOwnerFlags.NONE,
            acquire_daemon,
            on_daemon_acquired,
            make_proxy
        );

        active.connect(() => {
            done = true;
            if ((loop != null) && loop.is_running()) {
                loop.quit();
            }
        });

        if (loop != null) {
            loop.run();
        } else {
            while (!done) {
                MainContext.default().iteration(false);
            }
        }
    }

    private void acquire_daemon(DBusConnection conn) {
        daemon = new Daemon(conn);
    }

    private void on_daemon_acquired() {
        if (proxy != null) {
            proxy.stop();
            proxy = null;
        }
        daemon.notified.connect((id, replaced) => notified(id, replaced));
        daemon.resolved.connect((id, reason) => resolved(id, reason));
        items_changed(0, 0, notifications.length());
        active();
    }

    private void make_proxy() {
        proxy = new Proxy();
        proxy.notified.connect((id, replaced) => notified(id, replaced));
        proxy.resolved.connect((id, reason) => resolved(id, reason));
        items_changed(0, 0, notifications.length());
        active();
    }
}
