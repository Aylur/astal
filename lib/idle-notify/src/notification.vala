namespace AstalIdleNotify {

public class Notification : Object {

  private ExtIdleNotificationV1 notification;

  /*
   * Emitted when the seat has been inactive for at least the duration of the timout of this Notification
   */
  public signal void idled();

  /*
   * Emitted when the seat stops being inactive
   */
  public signal void resumed();

  private void handle_idled() {
    this.idled();
  }
  
  private void handle_resumed() {
    this.resumed();
  }

  private const ExtIdleNotificationV1Listener notification_listener = {
    handle_idled,
    handle_resumed 
  };

  internal Notification(owned ExtIdleNotificationV1 notification) {
    this.notification = (owned)notification;
    this.notification.add_listener(notification_listener, this);
  }

}
}
