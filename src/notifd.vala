namespace AstalNotifd {
public enum ActiveType {
    DAEMON,
    PROXY,
}

public class Notifd : Object {
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
        daemon.notified.connect((id, replaced) => notified(id, replaced));
        daemon.resolved.connect((id, reason) => resolved(id, reason));
        daemon.notify.connect((prop) => {
            if (get_class().find_property(prop.name) != null) {
                notify_property(prop.name);
            }
        });
    }

    private void try_proxy() {
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
}
