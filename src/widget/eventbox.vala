namespace Astal {
public class EventBox : Gtk.EventBox {
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
                this.set_state_flags(Gtk.StateFlags.PRELIGHT, false);
                hover(event);
            }
        });

        leave_notify_event.connect((self, event) => {
            if (event.window == self.get_window() &&
                event.detail != Gdk.NotifyType.INFERIOR) {
                this.unset_state_flags(Gtk.StateFlags.PRELIGHT);
                hover_lost(event);
            }
        });

        button_press_event.connect((event) => {
            // TODO: abstract event for easier use
            click(event);
        });

        button_release_event.connect((event) => {
            // TODO: abstract event for easier use
            click_release(event);
        });
    }
}
}
