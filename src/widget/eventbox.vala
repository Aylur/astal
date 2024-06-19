namespace Astal {
public class EventBox : Gtk.EventBox {
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
                var hover_event = HoverEvent();
                hover_event.lost = false;
                hover_event.time = event.time;
                hover_event.x = event.x;
                hover_event.y = event.y;
                hover_event.modifier = event.state;
                hover_event.mode = event.mode;
                hover_event.detail = event.detail;
                hover(hover_event);
            }
        });

        leave_notify_event.connect((self, event) => {
            if (event.window == self.get_window() &&
                event.detail != Gdk.NotifyType.INFERIOR) {
                this.unset_state_flags(Gtk.StateFlags.PRELIGHT);
                var hover_event = HoverEvent();
                hover_event.lost = true;
                hover_event.time = event.time;
                hover_event.x = event.x;
                hover_event.y = event.y;
                hover_event.modifier = event.state;
                hover_event.mode = event.mode;
                hover_event.detail = event.detail;
                hover_lost(hover_event);
            }
        });

        button_press_event.connect((event) => {
            var click_event = ClickEvent();
            click_event.release = false;
            click_event.time = event.time;
            click_event.x = event.x;
            click_event.y = event.y;
            click_event.button = (MouseButton)event.button;
            click_event.modifier = event.state;
            click(click_event);
        });

        button_release_event.connect((event) => {
            var click_event = ClickEvent();
            click_event.release = true;
            click_event.x = event.x;
            click_event.y = event.y;
            click_event.button = (MouseButton)event.button;
            click_event.modifier = event.state;
            click_release(click_event);
        });

        scroll_event.connect((event) => {
            var scroll_event = ScrollEvent();
            scroll_event.time = event.time;
            scroll_event.x = event.x;
            scroll_event.y = event.y;
            scroll_event.modifier = event.state;
            scroll_event.direction = event.direction;
            scroll_event.delta_x = event.delta_x;
            scroll_event.delta_y = event.delta_y;
            scroll(scroll_event);
        });

        motion_notify_event.connect((event) => {
            var motion_event = MotionEvent();
            motion_event.time = event.time;
            motion_event.x = event.x;
            motion_event.y = event.y;
            motion_event.modifier = event.state;
            motion(motion_event);
        });
    }
}

public struct MotionEvent {
    uint time;
    double x;
    double y;
    Gdk.ModifierType modifier;
}
}
