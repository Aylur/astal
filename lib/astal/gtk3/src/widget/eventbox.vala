/**
 * EventBox is a [class@Gtk.EventBox] subclass which is meant to fix an issue with its
 * [signal@Gtk.Widget::enter_notify_event] and [signal@Gtk.Widget::leave_notify_event] when nesting EventBoxes
 *
 * Its css selector is `eventbox`.
 */
public class Astal.EventBox : Gtk.EventBox {
    public signal void hover (HoverEvent event);
    public signal void hover_lost (HoverEvent event);
    public signal void click (ClickEvent event);
    public signal void click_release (ClickEvent event);
    public signal void scroll (ScrollEvent event);
    public signal void motion (MotionEvent event);

    static construct {
        set_css_name("eventbox");
    }

    construct {
        add_events(Gdk.EventMask.SCROLL_MASK);
        add_events(Gdk.EventMask.SMOOTH_SCROLL_MASK);
        add_events(Gdk.EventMask.POINTER_MOTION_MASK);

        enter_notify_event.connect((self, event) => {
            if (event.window == self.get_window() &&
                event.detail != Gdk.NotifyType.INFERIOR) {
                this.set_state_flags(Gtk.StateFlags.PRELIGHT, false);
                hover(HoverEvent(event) { lost = false });
            }
        });

        leave_notify_event.connect((self, event) => {
            if (event.window == self.get_window() &&
                event.detail != Gdk.NotifyType.INFERIOR) {
                this.unset_state_flags(Gtk.StateFlags.PRELIGHT);
                hover_lost(HoverEvent(event) { lost = true });
            }
        });

        button_press_event.connect((event) => {
            click(ClickEvent(event) { release = false });
        });

        button_release_event.connect((event) => {
            click_release(ClickEvent(event) { release = true });
        });

        scroll_event.connect((event) => {
            scroll(ScrollEvent(event));
        });

        motion_notify_event.connect((event) => {
            motion(MotionEvent(event));
        });
    }
}

/**
 * Struct for [struct@Gdk.EventMotion]
 */
public struct Astal.MotionEvent {
    uint time;
    double x;
    double y;
    Gdk.ModifierType modifier;

    public MotionEvent(Gdk.EventMotion event) {
        this.time = event.time;
        this.x = event.x;
        this.y = event.y;
        this.modifier = event.state;
    }
}
