public enum AstalNotifd.Urgency {
    LOW = 0,
    NORMAL = 1,
    CRITICAL = 2,
}

public enum AstalNotifd.ClosedReason {
    EXPIRED = 1,
    DISMISSED_BY_USER = 2,
    CLOSED = 3,
    UNDEFINED = 4;

    internal string to_string() {
        switch (this) {
            case EXPIRED:
                return "expired";
            case DISMISSED_BY_USER:
                return "dismissed-by-user";
            case CLOSED:
                return "closed";
            case UNDEFINED:
                return "undefined";
        }

        return_val_if_reached(null);
    }
}

public enum AstalNotifd.State {
    DRAFT,
    SENT,
    RECEIVED,
}
