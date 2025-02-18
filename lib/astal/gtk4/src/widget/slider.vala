public class Astal.Slider : Gtk.Scale {
    private Gtk.EventControllerLegacy controller;
    private bool dragging;

    construct {
        if (adjustment == null)
            adjustment = new Gtk.Adjustment(0,0,0,0,0,0);

        if (max == 0 && min == 0) {
            max = 1;
        }

        if (step == 0) {
            step = 0.05;
        }

        if (page == 0) {
            page = 0.01;
        }

        controller = new Gtk.EventControllerLegacy();
        add_controller(controller);
        controller.event.connect((event) => {
            var type = event.get_event_type();
            if (type == Gdk.EventType.BUTTON_PRESS ||
                type == Gdk.EventType.KEY_PRESS ||
                type == Gdk.EventType.TOUCH_BEGIN) {
                dragging = true;
            }
            if (type == Gdk.EventType.BUTTON_RELEASE ||
                type == Gdk.EventType.KEY_RELEASE ||
                type == Gdk.EventType.TOUCH_END) {
                dragging = false;
            }
        });
    }

    /**
     * Value of this slider. Defaults to `0`.
     */
    public double value {
        get { return adjustment.value; }
        set { if (!dragging) adjustment.value = value; }
    }

    /**
     * Minimum possible value of this slider. Defaults to `0`.
     */
    public double min {
        get { return adjustment.lower; }
        set { adjustment.lower = value; }
    }

    /**
     * Maximum possible value of this slider. Defaults to `1`.
     */
    public double max {
        get { return adjustment.upper; }
        set { adjustment.upper = value; }
    }

    /**
     * Size of step increments. Defaults to `0.05`.
     */
    public double step {
        get { return adjustment.step_increment; }
        set { adjustment.step_increment = value; }
    }

    /**
     * Size of page increments. Defaults to `0.01`.
     */
    public double page {
        get { return adjustment.page_increment; }
        set { adjustment.page_increment = value; }
    }
}
