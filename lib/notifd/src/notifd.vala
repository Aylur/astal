namespace AstalNotifd {
/**
 * Get the singleton instance of [class@AstalNotifd.Notifd]
 */
public Notifd get_default() {
    return Notifd.get_default();
}
}

/**
 * The Notification daemon.
 *
 * This class queues up to become the next daemon, while acting as a proxy in the meantime.
 */
public class AstalNotifd.Notifd : Object {
    internal static Settings settings;

    private List<weak Notification> fallback_list = new List<weak Notification>();
    private static Notifd _instance;

    /**
     * Get the singleton instance
     */
    public static Notifd get_default() {
        if (_instance == null) _instance = new Notifd();
        return _instance;
    }

    private Daemon daemon;
    private Proxy proxy;

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
     * Indicate to frontends to not show popups to the user.
     * This property does not have any effect on its own, its merely
     * a value to use between the daemon process and proxies for frontends to use.
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
            return fallback_list;
        }
    }

    /**
     * Gets the [class@AstalNotifd.Notification] with id or null if there is no such Notification.
     */
    public Notification? get_notification(uint id) {
        if (proxy != null) return proxy.get_notif(id);
        if (daemon != null) return daemon.get_notif(id);
        return null;
    }

    /**
     * Emitted when the daemon receives a [class@AstalNotifd.Notification].
     *
     * @param id The ID of the Notification.
     * @param replaced Indicates if an existing Notification was replaced.
     */
    public signal void notified(uint id, bool replaced) {
        notify_property("notifications");
    }

    /**
     * Emitted when a [class@AstalNotifd.Notification] is resolved.
     *
     * @param id The ID of the Notification.
     * @param reason The reason how the Notification was resolved.
     */
    public signal void resolved(uint id, ClosedReason reason) {
        notify_property("notifications");
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
        active();
    }

    private void make_proxy() {
        proxy = new Proxy();
        proxy.notified.connect((id, replaced) => notified(id, replaced));
        proxy.resolved.connect((id, reason) => resolved(id, reason));
        active();
    }
}
