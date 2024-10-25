/**
 * This button has no extra functionality on top if its base [class@Gtk.Button] class.
 *
 * The purpose of this Button subclass is to have a destructable
 * struct as the argument in GJS event handlers.
 */
public class Astal.Button : Gtk.Button {
    public signal void hover (HoverEvent event);
    public signal void hover_lost (HoverEvent event);
    public signal void click (ClickEvent event);
    public signal void click_release (ClickEvent event);
    public signal void scroll (ScrollEvent event);

    construct {
        add_events(Gdk.EventMask.SCROLL_MASK);
        add_events(Gdk.EventMask.SMOOTH_SCROLL_MASK);

        enter_notify_event.connect((self, event) => {
            hover(HoverEvent(event) { lost = false });
        });

        leave_notify_event.connect((self, event) => {
            hover_lost(HoverEvent(event) { lost = true });
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
    }
}

public enum Astal.MouseButton {
    PRIMARY = 1,
    MIDDLE = 2,
    SECONDARY = 3,
    BACK = 4,
    FORWARD = 5,
}

/**
 * Struct for [struct@Gdk.EventButton]
 */
public struct Astal.ClickEvent {
    bool release;
    uint time;
    double x;
    double y;
    Gdk.ModifierType modifier;
    MouseButton button;

    public ClickEvent(Gdk.EventButton event) {
        this.time = event.time;
        this.x = event.x;
        this.y = event.y;
        this.button = (MouseButton)event.button;
        this.modifier = event.state;
    }
}

/**
 * Struct for [struct@Gdk.EventCrossing]
 */
public struct Astal.HoverEvent {
    bool lost;
    uint time;
    double x;
    double y;
    Gdk.ModifierType modifier;
    Gdk.CrossingMode mode;
    Gdk.NotifyType detail;

    public HoverEvent(Gdk.EventCrossing event) {
        this.time = event.time;
        this.x = event.x;
        this.y = event.y;
        this.modifier = event.state;
        this.mode = event.mode;
        this.detail = event.detail;
    }
}

/**
 * Struct for [struct@Gdk.EventScroll]
 */
public struct Astal.ScrollEvent {
    uint time;
    double x;
    double y;
    Gdk.ModifierType modifier;
    Gdk.ScrollDirection direction;
    double delta_x;
    double delta_y;

    public ScrollEvent(Gdk.EventScroll event) {
        this.time = event.time;
        this.x = event.x;
        this.y = event.y;
        this.modifier = event.state;
        this.direction = event.direction;
        this.delta_x = event.delta_x;
        this.delta_y = event.delta_y;
    }
}
