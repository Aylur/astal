namespace AstalIdleNotify {

public class Notification : Object {

  private ExtIdleNotificationV1 notification;

  /**
   * Whether or not the system has been idle for at least the amount given by the timeout.
   */
  public bool idle { get; private set; default = false;}

  /**
   * the timeout this notification has been constructed with.
   */
  public uint timeout { get; construct; }

  /**
   * Emitted when the seat has been inactive for at least the duration of the timout of this Notification
   */
  public signal void idled();

  /**
   * Emitted when the seat stops being inactive
   */
  public signal void resumed();

  private void handle_idled() {
    this.idle = true;
    this.idled();
  }
  
  private void handle_resumed() {
    this.idle = false;
    this.resumed();
  }

  private const ExtIdleNotificationV1Listener notification_listener = {
    handle_idled,
    handle_resumed 
  };

  internal Notification(owned ExtIdleNotificationV1 notification, uint timeout) {
    Object(timeout: timeout);
    this.notification = (owned)notification;
    this.notification.add_listener(notification_listener, this);
  }

}
}
