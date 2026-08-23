namespace AstalWl {
/**
 * Wraps the Wayland `ext_idle_notification_v1` interface.
 *
 * An idle notification reports when a [class@AstalWl.Seat] has been inactive
 * for a given duration and when activity resumes. Instances are created with
 * [method@AstalWl.Seat.get_idle_notification].
 */
public class IdleNotification : Object {
    private ExtIdleNotificationV1 notification;

    /**
     * The inactivity duration in milliseconds before this notification idles.
     */
    public uint timeout { get; construct; }

    /**
     * Whether the seat is currently idle.
     */
    public bool idle { get; private set; }

    /**
     * Emitted after the seat has been inactive for the timeout.
     */
    public signal void idled();

    /**
     * Emitted on the first activity after the seat has idled.
     */
    public signal void resumed();

    private void handle_idled(ExtIdleNotificationV1 ext_idle_notification_v1) {
        this.idle = true;
        idled();
    }

    private void handle_resumed(ExtIdleNotificationV1 ext_idle_notification_v1) {
        this.idle = false;
        resumed();
    }

    private const ExtIdleNotificationV1Listener notification_listener = {
        handle_idled,
        handle_resumed,
    };

    internal IdleNotification(owned ExtIdleNotificationV1 notification, uint timeout) {
        Object(timeout: timeout);
        this.notification = (owned) notification;
        this.notification.add_listener(notification_listener, this);
    }
}
}
