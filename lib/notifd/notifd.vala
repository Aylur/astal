/**
 * Get the singleton instance of [class@AstalNotifd.Notifd]
 */
namespace AstalNotifd {
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
    private static Notifd _instance;

    /**
     * Get the singleton instance
     */
    public static Notifd get_default() {
        if (_instance == null)
            _instance = new Notifd();

        return _instance;
    }

    private Daemon daemon;
    private DaemonProxy proxy;

    internal signal void active(ActiveType type);

    /**
     * Ignore the timeout specified by incoming notifications.
     *
     * By default notifications can specify a timeout in milliseconds
     * after which the daemon will resolve them even without user input.
     */
    public bool ignore_timeout {
        get {
            return proxy != null ? proxy.ignore_timeout : daemon.ignore_timeout;
        }
        set {
            if (proxy != null)
                proxy.ignore_timeout = value;
            else
                daemon.ignore_timeout = value;
        }
    }

    /**
     * Indicate to frontends to not show popups to the user.
     *
     * This property does not have any effect on its own, its merely
     * a value to use between the daemon process and proxies for frontends to use.
     */
    public bool dont_disturb {
        get {
            return proxy != null ? proxy.dont_disturb : daemon.dont_disturb;
        }
        set {
            if (proxy != null)
                proxy.dont_disturb = value;
            else
                daemon.dont_disturb = value;
        }
    }

    /**
     * List of currently unresolved notifications.
     */
    public List<weak Notification> notifications {
        owned get { return proxy != null ? proxy.notifications : daemon.notifications; }
    }

    /**
     * Gets the [class@AstalNotifd.Notification] with id or null if there is no such Notification.
     */
    public Notification get_notification(uint id) {
        return proxy != null ? proxy.get_notification(id) : daemon.get_notification(id);
    }

    internal string get_notification_json(uint id) {
        return get_notification(id).to_json_string();
    }

    /**
     * Emitted when the daemon receives a [class@AstalNotifd.Notification].
     *
     * @param id The ID of the Notification.
     * @param replaced Indicates if an existing Notification was replaced.
     */
    public signal void notified(uint id, bool replaced);

    /**
     * Emitted when a [class@AstalNotifd.Notification] is resolved.
     *
     * @param id The ID of the Notification.
     * @param reason The reason how the Notification was resolved.
     */
    public signal void resolved(uint id, ClosedReason reason);

    construct {
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
            if (loop != null && loop.is_running()) {
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
        daemon = new Daemon().register(conn);
    }

    private void on_daemon_acquired() {
        if (proxy != null) {
            proxy.stop();
            proxy = null;
        }
        daemon.notified.connect((id, replaced) => notified(id, replaced));
        daemon.resolved.connect((id, reason) => resolved(id, reason));
        daemon.notify.connect((prop) => {
            if (get_class().find_property(prop.name) != null) {
                notify_property(prop.name);
            }
        });
        active(ActiveType.DAEMON);
    }

    private void make_proxy() {
        proxy = new DaemonProxy();

        if (proxy.start()) {
            active(ActiveType.PROXY);
        } else {
            return;
        }

        proxy.notified.connect((id, replaced) => notified(id, replaced));
        proxy.resolved.connect((id, reason) => resolved(id, reason));
        proxy.notify.connect((prop) => {
            if (get_class().find_property(prop.name) != null) {
                notify_property(prop.name);
            }
        });
    }
}

internal enum AstalNotifd.ActiveType {
    DAEMON,
    PROXY,
}
