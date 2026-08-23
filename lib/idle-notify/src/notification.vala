namespace AstalIdleNotify {

public class Notification : Object {

  private ExtIdleNotificationV1 notification;

  public signal void idled();
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
