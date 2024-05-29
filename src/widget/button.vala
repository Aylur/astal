namespace Astal {
public class Button : Gtk.Button {
    public signal void hover (HoverEvent event);
    public signal void hover_lost (HoverEvent event);
    public signal void click (ClickEvent event);
    public signal void click_release (ClickEvent event);
    public signal void scroll (ScrollEvent event);

    construct {
        add_events(Gdk.EventMask.SCROLL_MASK);
        add_events(Gdk.EventMask.SMOOTH_SCROLL_MASK);

        enter_notify_event.connect((self, event) => {
            if (event.window == self.get_window() &&
                event.detail != Gdk.NotifyType.INFERIOR) {
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
    }
}

public enum MouseButton {
    PRIMARY = 0,
    MIDDLE,
    SECONDARY,
    BACK,
    FORWARD,
}

// these structs are here because gjs converts every event
// into a union Gdk.Event, which cannot be destructured
// and are not as convinent to work with as a struct
public struct ClickEvent {
    bool release;
    uint time;
    double x;
    double y;
    Gdk.ModifierType modifier;
    MouseButton button;
}

public struct HoverEvent {
    bool lost;
    uint time;
    double x;
    double y;
    Gdk.ModifierType modifier;
    Gdk.CrossingMode mode;
    Gdk.NotifyType detail;
}

public struct ScrollEvent {
    uint time;
    double x;
    double y;
    Gdk.ModifierType modifier;
    Gdk.ScrollDirection direction;
    double delta_x;
    double delta_y;
}
}
