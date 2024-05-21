namespace AstalNotifd {
public enum ActiveType {
    DAEMON,
    PROXY,
}

public class Notifd : Object {
    private Daemon daemon;
    private DaemonProxy proxy;

    private HashTable<uint, Notification> notifs =
        new HashTable<uint, Notification>((i) => i, (a, b) => a == b);

    public signal void active(ActiveType type);

    public string cache_directory {
        owned get {
            return proxy != null ? proxy.cache_directory : daemon.cache_directory;
        }
        set {
            if (proxy != null)
                proxy.cache_directory = value;
            else
                daemon.cache_directory = value;
        }
    }

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
        owned get { return notifs.get_values(); }
    }

    public uint[] notification_ids() throws Error {
        return proxy != null ? proxy.notification_ids() : daemon.notification_ids();
    }

    public Notification get_notification(uint id) {
        return notifs.get(id);
    }

    public string get_notification_json(uint id) {
        return notifs.get(id).to_json_string();
    }

    public signal void notified(uint id) {
        add_notification(id);
    }

    public signal void resolved(uint id, ClosedReason reason) {
        notifs.remove(id);
    }

    private void add_notification(uint id) {
        try {
            if (proxy != null) {
                var json = proxy.get_notification_json(id);
                notifs.set(id, Notification.from_json_string(json));
            } else {
                notifs.set(id, daemon.get_notification(id));
            }
        } catch (Error err) {
            warning("could not add notification: %s", err.message);
        }
    }

    construct {
        Bus.own_name(
            BusType.SESSION,
            "org.freedesktop.Notifications",
            BusNameOwnerFlags.NONE,
            try_daemon,
            () => {
                if (proxy != null) {
                    proxy.stop();
                    proxy = null;
                }
                active(ActiveType.DAEMON);
            },
            try_proxy
        );
    }

    private void try_daemon(DBusConnection conn) {
        daemon = new Daemon().register(conn);
        daemon.notified.connect((id) => notified(id));
        daemon.resolved.connect((id, reason) => resolved(id, reason));
        daemon.notify.connect((prop) => {
            if (get_class().find_property(prop.name) != null) {
                notify_property(prop.name);
            }
        });
        foreach (var n in daemon.notifications) {
            notifs.set(n.id, n);
        }
    }

    private void try_proxy() {
        proxy = new DaemonProxy();
        if (proxy.start()) {
            try {
                foreach (var id in proxy.notification_ids()) {
                    add_notification(id);
                }
            }
            catch (Error err) {
                warning("could not get notification ids: %s", err.message);
            }

            active(ActiveType.PROXY);
        } else {
            return;
        }

        proxy.notified.connect((id) => notified(id));
        proxy.resolved.connect((id, reason) => resolved(id, reason));
        proxy.notify.connect((prop) => {
            if (get_class().find_property(prop.name) != null) {
                notify_property(prop.name);
            }
        });
    }
}
}
