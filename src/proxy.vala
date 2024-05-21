namespace AstalNotifd {
[DBus (name = "org.freedesktop.Notifications")]
internal interface IDaemon : Object {
    public abstract bool ignore_timeout { get; set; }
    public abstract bool dont_disturb { get; set; }

    public abstract uint[] notification_ids() throws DBusError, IOError;
    public abstract string get_notification_json(uint id) throws DBusError, IOError;

    public signal void notified(uint id);
    public signal void resolved(uint id, ClosedReason reason);

    public abstract void emit_notified(uint id);
    public abstract void emit_resolved(uint id, ClosedReason reason);
    public abstract void emit_action_invoked(uint id, string action);
}

internal class DaemonProxy : Object {
    private HashTable<uint, Notification> notifs =
        new HashTable<uint, Notification>((i) => i, (a, b) => a == b);

    public List<weak Notification> notifications {
        owned get { return notifs.get_values(); }
    }

    public bool ignore_timeout {
        get { return proxy.ignore_timeout; }
        set { proxy.ignore_timeout = value; }
    }

    public bool dont_disturb {
        get { return proxy.dont_disturb; }
        set { proxy.dont_disturb = value; }
    }

    public uint[] notification_ids() throws DBusError, IOError {
        return proxy.notification_ids();
    }

    public Notification get_notification(uint id) {
        return notifs.get(id);
    }

    public signal void notified(uint id);
    public signal void resolved(uint id, ClosedReason reason);

    IDaemon proxy;
    List<ulong> ids = new List<ulong>();

    public void stop() {
        if (ids.length() > 0) {
            foreach (var id in ids)
                SignalHandler.disconnect(proxy, id);
        }
    }

    public bool start() {
        try  {
            var bus = Bus.get_sync(BusType.SESSION, null);
            var variant = bus.call_sync(
                "org.freedesktop.Notifications",
                "/org/freedesktop/Notifications",
                "org.freedesktop.Notifications",
                "GetServerInformation",
                null,
                null,
                DBusCallFlags.NONE,
                -1,
                null);

            var name = variant.get_child_value(0).get_string();
            var vendor = variant.get_child_value(1).get_string();
            var version = variant.get_child_value(2).get_string();

            var running = name == Daemon.name
              && vendor == Daemon.vendor
              && version == Daemon.version;

            if (running) {
                setup_proxy();
                return true;
            } else {
                critical("cannot get proxy: %s is already running", name);
            }
        } catch (Error err) {
            critical("cannot get proxy: %s", err.message);
        }
        return false;
    }

    private void setup_proxy() throws Error {
        proxy = Bus.get_proxy_sync(
            BusType.SESSION,
            "org.freedesktop.Notifications",
            "/org/freedesktop/Notifications"
        );

        foreach (var id in proxy.notification_ids())
            add_notification(id);

        ids.append(proxy.notify.connect((pspec) => {
            if (get_class().find_property(pspec.name) != null)
                notify_property(pspec.name);
        }));

        ids.append(proxy.notified.connect((id) => {
            add_notification(id);
            notified(id);
        }));

        ids.append(proxy.resolved.connect((id, reason) => {
            notifs.remove(id);
            resolved(id, reason);
        }));
    }

    private void add_notification(uint id) {
        try {
            var n = Notification.from_json_string(proxy.get_notification_json(id));
            proxy.resolved.connect((id, reason) => n.resolved(reason));
            n.dismissed.connect(() => proxy.emit_resolved(id, ClosedReason.DISMISSED_BY_USER));
            n.invoked.connect((action) => proxy.emit_action_invoked(id, action));
            notifs.set(id, n);
        } catch (Error err) {
            critical(err.message);
        }
    }
}
}
