namespace AstalNotifd {
[DBus(name = "org.freedesktop.Notifications")]
internal interface NotificationsProxy : DBusProxy {
    public abstract async uint Notify(
        string app_name,
        uint replaces_id,
        string app_icon,
        string summary,
        string body,
        string[] actions,
        HashTable<string, Variant> hints,
        int expire_timeout
    ) throws DBusError, IOError;

    public abstract async void close_notification(uint id) throws DBusError, IOError;

    public signal void notification_closed(uint id, uint reason);
    public signal void action_invoked(uint id, string action);
}

public async void send_notification(Notification notification) throws IOError, DBusError {
    var n = notification;

    if (n.state != State.DRAFT) {
        throw new IOError.INVALID_ARGUMENT("cannot send notification: not a draft");
    }

    NotificationsProxy proxy = yield Bus.get_proxy(
        BusType.SESSION,
        "org.freedesktop.Notifications",
        "/org/freedesktop/Notifications"
    );

    var actions_len = n.actions.length();
    var actions = new string[actions_len * 2];

    int i = 0;
    foreach (var action in n.actions) {
        actions[i++] = action.id;
        actions[i++] = action.label;
    }

    var hints = new HashTable<string, Variant>(str_hash, str_equal);
    var iter = new VariantIter(n.hints);
    string? key;
    Variant? variant;
    while (iter.next("{sv}", out key, out variant)) {
        hints.set(key, variant);
    }

    if (n.app_name == "") {
        n.app_name = Environment.get_application_name() ?? Environment.get_prgname() ?? "Notifd";
    }

    var n_id = yield proxy.Notify(
        n.app_name,
        n.id,
        n.app_icon,
        n.summary,
        n.body,
        actions,
        hints,
        n.expire_timeout
    );

    ulong closed_handler = 0, invoked_handler = 0;
    closed_handler = proxy.notification_closed.connect((id, reason) => {
            if (id == n_id) n.resolved(reason);
        });

    invoked_handler = proxy.action_invoked.connect((id, action_id) => {
            if (id == n_id) n.invoked(action_id);
        });

    n.resolved.connect(() => {
            proxy.disconnect(closed_handler);
            proxy.disconnect(invoked_handler);
            proxy = null;
        });

    // should we resolve it if the server stops?
    n.id = n_id;
    n.state = State.SENT;
}
}
