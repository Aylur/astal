[DBus(name = "org.freedesktop.Notifications")]
internal interface AstalNotifd.DaemonProxy : DBusProxy {
    public signal void notified(uint id, bool replaced);
    public signal void resolved(uint id, ClosedReason reason);

    public abstract async Variant get_notification(uint id) throws Error;
    public abstract async void emit_resolved(uint id, ClosedReason reason) throws Error;
    public abstract async void emit_action_invoked(uint id, string action) throws Error;
}

internal class AstalNotifd.Proxy : Object {
    private List<weak Notification> notification_list =
        new List<weak Notification>();

    private HashTable<uint, Notification> notifs =
        new HashTable<uint, Notification>((i) => i, (a, b) => a == b);

    public signal void notified(uint id, bool replaced);
    public signal void resolved(uint id, ClosedReason reason);

    private DaemonProxy proxy;
    private ulong notified_handler;
    private ulong resolved_handler;

    internal Notification? get_notif(uint id) {
        return notifs.get(id);
    }

    internal List<weak Notification> notifications {
        get { return notification_list; }
    }

    public void stop() {
        SignalHandler.disconnect(proxy, notified_handler);
        SignalHandler.disconnect(proxy, resolved_handler);
    }

    construct {
        try {
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
                null
            );

            var name = variant.get_child_value(0).get_string();
            var vendor = variant.get_child_value(1).get_string();
            var version = variant.get_child_value(2).get_string();

            var running = (name == Daemon.name)
                && (vendor == Daemon.vendor)
                && (version == Daemon.version);

            if (running) {
                setup_proxy();
            } else {
                critical(@"cannot get proxy: $name is already running");
            }
        } catch (Error err) {
            critical(@"cannot get proxy: $(err.message)");
        }
    }

    private void setup_proxy() throws Error {
        proxy = Bus.get_proxy_sync(
            BusType.SESSION,
            "org.freedesktop.Notifications",
            "/org/freedesktop/Notifications"
        );

        var iter = new VariantIter(Notifd.settings.get_value("notifications"));
        Variant? variant;
        while (iter.next("v", out variant)) {
            add_notification(new Notification.deserialize(variant));
        }

        notified_handler = proxy.notified.connect((id, replaced) => {
            proxy.get_notification.begin(id, (_, res) => {
                try {
                    var v = proxy.get_notification.end(res);
                    var n = new Notification.deserialize(v);
                    add_notification(n);
                    notified(id, replaced);
                } catch (Error error) {
                    critical(error.message);
                }
            });
        });

        resolved_handler = proxy.resolved.connect((id, reason) => {
            var n = notifs.get(id);
            if (n != null) {
                n.resolved(reason);
                notifs.remove(id);
                notification_list = notifs.get_values().copy();
                resolved(id, reason);
            }
        });
    }

    private void add_notification(Notification n) {
        n.invoked.connect((action) => {
            proxy.emit_action_invoked.begin(n.id, action);
            if (!n.resident) proxy.emit_resolved.begin(n.id, ClosedReason.CLOSED);
        });
        n.dismissed.connect(() => proxy.emit_resolved.begin(n.id, ClosedReason.DISMISSED_BY_USER));
        n.expired.connect(() => proxy.emit_resolved.begin(n.id, ClosedReason.EXPIRED));
        n.state = State.RECEIVED;
        notifs.set(n.id, n);
        notification_list = notifs.get_values().copy();
    }
}
