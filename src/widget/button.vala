namespace Astal {
public class Button : Gtk.Button {
    public signal void hover (Gdk.EventCrossing event);
    public signal void hover_lost (Gdk.EventCrossing event);
    public signal void click (Gdk.EventButton event);
    public signal void click_release (Gdk.EventButton event);

    construct {
        add_events(Gdk.EventMask.SCROLL_MASK);
        add_events(Gdk.EventMask.SMOOTH_SCROLL_MASK);

        enter_notify_event.connect((self, event) => {
            if (event.window == self.get_window() &&
                event.detail != Gdk.NotifyType.INFERIOR) {
                hover(event);
            }
        });

        leave_notify_event.connect((self, event) => {
            if (event.window == self.get_window() &&
                event.detail != Gdk.NotifyType.INFERIOR) {
                hover_lost(event);
            }
        });

        button_press_event.connect((event) => {
            click(event);
        });

        button_release_event.connect((event) => {
            click_release(event);
        });
    }
}
}
