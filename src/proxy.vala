namespace AstalNotifd {
[DBus (name = "org.freedesktop.Notifications")]
internal interface IDaemon : Object {
    public abstract string cache_directory { owned get; set; }
    public abstract bool ignore_timeout { get; set; }
    public abstract bool dont_disturb { get; set; }

    public abstract uint[] notification_ids() throws DBusError, IOError;
    public abstract string get_notification_json(uint id) throws DBusError, IOError;

    public signal void notified(uint id);
    public signal void resolved(uint id, ClosedReason reason);
}

internal class DaemonProxy : Object {
    public string cache_directory {
        owned get { return proxy.cache_directory; }
        set { proxy.cache_directory = value; }
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

    public string get_notification_json(uint id) throws DBusError, IOError {
        return proxy.get_notification_json(id);
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
                proxy = Bus.get_proxy_sync(
                    BusType.SESSION,
                    "org.freedesktop.Notifications",
                    "/org/freedesktop/Notifications"
                );

                ids.append(proxy.notified.connect((id) => notified(id)));
                ids.append(proxy.resolved.connect((id, reason) => resolved(id, reason)));
                ids.append(proxy.notify.connect((pspec) => {
                    if (get_class().find_property(pspec.name) != null)
                        notify_property(pspec.name);
                }));
                return true;
            } else {
                critical("cannot get proxy: %s is already running", name);
            }
        } catch (Error err) {
            critical("cannot get proxy: %s", err.message);
        }
        return false;
    }
}
}
