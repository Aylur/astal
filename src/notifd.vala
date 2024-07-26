namespace AstalNotifd {
public Notifd get_default() {
    return Notifd.get_default();
}

public class Notifd : Object {
    private static Notifd _instance;
    public static Notifd get_default() {
        if (_instance == null)
            _instance = new Notifd();

        return _instance;
    }

    private Daemon daemon;
    private DaemonProxy proxy;

    public signal void active(ActiveType type);

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

    public List<weak Notification> notifications {
        owned get { return proxy != null ? proxy.notifications : daemon.notifications; }
    }

    public uint[] notification_ids() throws Error {
        return proxy != null ? proxy.notification_ids() : daemon.notification_ids();
    }

    public Notification get_notification(uint id) {
        return proxy != null ? proxy.get_notification(id) : daemon.get_notification(id);
    }

    public string get_notification_json(uint id) {
        return get_notification(id).to_json_string();
    }

    public signal void notified(uint id, bool replaced);
    public signal void resolved(uint id, ClosedReason reason);

    construct {
        // hack to make it synchronous
        var loop = new MainLoop();

        Bus.own_name(
            BusType.SESSION,
            "org.freedesktop.Notifications",
            BusNameOwnerFlags.NONE,
            acquire_daemon,
            on_daemon_acquired,
            make_proxy
        );

        active.connect(() => {
            if (loop.is_running())
                loop.quit();
        });

        loop.run();
    }

    private void acquire_daemon(DBusConnection conn) {
        daemon = new Daemon().register(conn);
        daemon.notified.connect((id, replaced) => notified(id, replaced));
        daemon.resolved.connect((id, reason) => resolved(id, reason));
        daemon.notify.connect((prop) => {
            if (get_class().find_property(prop.name) != null) {
                notify_property(prop.name);
            }
        });
    }

    private void on_daemon_acquired() {
        if (proxy != null) {
            proxy.stop();
            proxy = null;
        }
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

public enum ActiveType {
    DAEMON,
    PROXY,
}
}
